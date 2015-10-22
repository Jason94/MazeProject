(defun point (x y)
	"Construct a point of the given x & y."
	(list :x x :y y))


(defvar *lst* (make-array '(5 5) :initial-element (point -1 -1)))
	
	;(make-array 5 :initial-element 
		;(make-array 5 :initial-element (point -1 -1))))

(defun norm-list ()
	(loop for y from 0 to 4
				do (loop for x from 0 to 4
							do (setf (aref *lst* y x) (point x y)))))
