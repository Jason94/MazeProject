;;;; Helper Functions ;;;;

(defun inc (x)
	(+ x 1))
	
(defun dec (x)
	(- x 1))
	
(defun random-elt (lst)
	(nth (random (length lst)) lst))
	
;;;; Data-Types ;;;;

;;; Points & Directions ;;; 
(defun point (x y)
	"Construct a point of the given x & y."
	(list :x x :y y))
	
(defun x (pnt)
	(getf pnt :x))
	
(defun y (pnt)
	(getf pnt :y))
	
(defvar *directions* 
	;For ease of use, store a list of each direction.
	(list :N :W :S :E))
	
(defun dir-from (pnt-from pnt-to)
	"Returns the direction from pnt-from to pnt-to. Will only work for
	 cardinal adjacent points."
	(cond
		((< (x pnt-from) (x pnt-to)) :E)
		((> (x pnt-from) (x pnt-to)) :W)
		((< (y pnt-from) (y pnt-to)) :S)
		((> (y pnt-from) (y pnt-to)) :N)))
	 
(defun point-in-dir (pnt dir)
	"Returns the point in dir from pnt."
	(let ((x (getf pnt :x))
				(y (getf pnt :y)))
		(case dir 
			(:N (point x (dec y)))
			(:S (point x (inc y)))
			(:W (point (dec x) y))
			(:E (point (inc x) y)))))

(defun adjacent-points (pnt)
	"Return points adjacent to pnt in the four cardinal directions."
	(map 'list #'(lambda (dir) (point-in-dir pnt dir)) *directions*))

(defun safe-point-p (pnt N)
	"Return if a point is in an NxN grid."
	(let ((x (getf pnt :x))
		  (y (getf pnt :y)))
		(and
			(>= x 0)
			(>= y 0)
			(< x N)
			(< y N))))

(defun sanitize-points (pnts N)
	"Takes a list of points, and returns those within an NxN grid."
	(remove-if-not (lambda (pnt) (safe-point-p pnt N)) pnts))

;;; Mazes ;;;

;; A maze is stored as a vector of vectors of columns of cells.
;; A cell in a maze is a p-list of :N, :W, and :loc.
;; T represents a wall. nil represents an opening.

(defun empty-maze (N)
	"Create an NxN maze of cells with all walls."
	;First create the maze.
	(let ((base-maze (make-array (list N N)
						:initial-element (list :N T :W T))))
		;Insert the loc's.
		(loop for x from 0 to (dec N)
			do (loop for y from 0 to (dec N)
				 do (setf (aref base-maze y x) 
						  (list :N T :W T :loc (point x y)))))
		 ;Return the base maze.
		 base-maze))
		 
(defun connect-points (maze pnt-a pnt-b)
	"Connect pnt-a and pnt-b. Non-immutable on maze. 
	 Returns a copy of maze."
	(let ((dir (dir-from pnt-a pnt-b)))
		(case dir
			(:N (setf (getf (aref maze (y pnt-a) (x pnt-a)) :N) nil))
			(:W (setf (getf (aref maze (y pnt-a) (x pnt-a)) :W) nil))
			(:S (setf (getf (aref maze (y pnt-b) (x pnt-b)) :N) nil))
			(:E (setf (getf (aref maze (y pnt-b) (x pnt-b)) :W) nil)))
		maze))
		 
(defun connect-point-dir (maze pnt dir)
	"Connect pnt to the pnt in dir."
	(connect-points maze pnt (point-in-dir pnt dir)))

(defun print-maze (maze)
	(let ((N (array-dimension maze 0)))
		(loop for i from 0 below N
			do (format  t "__"))
		(format t "_~%")
		(loop for y from 0 below N
			do (loop for x from 0 below N
						do (let* ((cell (aref maze y x))
											(W (getf cell :W))
											(S (or (eql (dec N) y)
														 (getf (aref maze (inc y) x) :N))))
									(if W
										(format t "|")
										(format t " "))
									(if S
										(format t "_")
										(format t " "))))
					(format t "|~%"))))

(defun points-connected-p (maze pnt-a pnt-b)
	"Return T if adjacent points pnt-a and pnt-b are connected."
	(not (case (dir-from pnt-a pnt-b)
				(:N (getf (aref maze (y pnt-a) (x pnt-a)) :N))
				(:W (getf (aref maze (y pnt-a) (x pnt-a)) :W))
				(:S (getf (aref maze (y pnt-b) (x pnt-b)) :N))
				(:E (getf (aref maze (y pnt-b) (x pnt-b)) :W)))))

(defun neighbors-of (maze pnt)
	"Return the neighbor cells of pnt in maze."
	(let* ((N (array-dimension maze 0))
				 (raw-neighbors (sanitize-points (adjacent-points pnt) N)))
		(remove-if-not 
			#'(lambda (pnt-b) (points-connected-p maze pnt pnt-b))
			raw-neighbors)))
			
(defun num-connections (maze pnt)
	"Return the number of connected cells to pnt."
	(length (neighbors-of maze pnt)))

(defun dead-end-p (maze pnt)
	"Check if the cell at pnt in the maze is a dead end."
	(equal 1 (length (neighbors-of maze pnt))))
	
(defun dead-ends (maze)
	"Return a list of all the dead ends."
	(let ((N (array-dimension maze 0))
				(dead-ends '()))
		(loop for y from 0 below N
			do (loop for x from 0 below N
						when (dead-end-p maze (point x y))
							do (setf dead-ends (cons (point x y) dead-ends))))
		dead-ends))

;;;; Maze Generation Algorithms ;;;;

(defun growing-tree (N &optional (heuristic-fun #'random-elt)
										(maze (empty-maze N))
										(active-set -1)
										(visited '()))
	(case active-set
		(-1
			; Start of algorithm. Pick a random element and add to active set.
			(let* ((x (random N))
						 (y (random N))
						 (new-active-set (list (point x y))))
				 (growing-tree N heuristic-fun maze new-active-set new-active-set)))
		('()
			; End of algorithm. Return the maze.
			maze)
		(t
			; Iteration of algorithm. (1) Use the heuristic to pick a cell from
			; the active set. Connect it to a neighbor. Add neighbor to the
			; active set. (2) Pick a cell with no neighbors. Remove cell from
			; the active set and re-iterate.
			(let* ((active-cell (funcall heuristic-fun active-set))
						 (neighbors (sanitize-points (adjacent-points active-cell) N))
						 (unvisited-neighbors 
								(remove-if 
									#'(lambda (pnt) (find pnt visited :test #'equal))
									neighbors)))
				(if unvisited-neighbors
					; Neighbors exist. Pick at random, connect, and iterate.
					(let* ((neighbor (random-elt unvisited-neighbors))
								 (new-active-set (cons neighbor active-set)))
						(connect-points maze active-cell neighbor)
						(growing-tree N heuristic-fun maze new-active-set (cons neighbor visited)))
					; No neighbors exist. Remove from active set and iterate.
					(let ((new-active-set (remove active-cell active-set :test #'equal)))
						(growing-tree N heuristic-fun maze new-active-set visited)))))))

(defun recursive-backtracking (N)
	(growing-tree N #'(lambda (lst) (car lst))))
	
(defun oldest-active (N)
	(growing-tree N #'(lambda (lst) (car (reverse lst)))))

;;;; Maze Analysis Algorithms ;;;;

(defun corridor-lengths (maze)
	"Return a list of each corridor size in the maze."
	; First mark each cell with a unique corridor ID.
	(let ((N (array-dimension maze 0)))
		(loop for x from 0 below N
			do (loop for y from 0 below N
						do (setf
										(getf (aref maze y x) :coll-id)
										(+ y (* N x)))))
		maze))
	
;	(let ((dead-end-cells (dead-ends maze)))
;		(loop for i from 0 below (length dead-end-cells)
;			do (let ((x (x (nth i dead-end-cells)))
;							 (y (y (nth i dead-end-cells))))
;						(setf
;							(getf (aref maze y x) :coll-id)
;							i)))
;		maze))
