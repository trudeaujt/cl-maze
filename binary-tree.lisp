(load "~/code/maze/maze.lisp")

(defun binary-tree-maze ()
  (let ((g (make-instance 'maze-grid)))
    (initialize g :rows 3 :cols 3)
    (map-vector (lambda (cell)
                  (let ((target (sample (remove nil (list (north cell)
                                                          (east cell))))))
                    (when target
                      (link cell :to target))))
                (grid g))
    g))

(defun for-each-row (fn array)
  (loop for i from 0 below (array-dimension array 0)
        do (let ((row (loop for j from 0 below (array-dimension array 1)
                            collect (aref array i j))))
             (funcall fn row))))

(defparameter *cell-width* 6)
(defparameter *cell-height* 4)

(defmethod render-maze ((maze maze-grid))
  (let* ((rows (grid-rows maze))
         (cols (grid-cols maze))
         (out-rows (* rows *cell-height*))
         (out-cols (* cols *cell-width*))
         (buffer (make-array (list out-rows out-cols)
                             :element-type 'character
                             :initial-element #\Space)))
    ;;Draw outside walls
    (dotimes (c out-cols)
      (setf (aref buffer 0 c) #\-))
    (dotimes (r out-rows)
      (setf (aref buffer r 0) #\|))
    ;;Draw a wall or opening for each cell depending on its links
    ;;TODO maybe need to shift the first cell down a row and increase the size of the output by one row
    (dotimes (r rows)
      (dotimes (c cols)
        (let* ((cell (aref (grid maze) r c))
               (base-row (* r *cell-height*))
               (base-col (* c *cell-width*)))
          ;;Draw cell south
          (dotimes (i *cell-width*)
            (setf (aref buffer (+ base-row (1- *cell-height*)) (+ base-col i)) 
                  (cond ((linkedp cell :to (south cell)) #\Space)
                        (t #\-))))
          ;;Draw cell south
          (dotimes (i *cell-height*)
            (setf (aref buffer (+ base-row i) (+ base-col (- *cell-width* 1)))
                  (cond ((linkedp cell :to (east cell)) #\Space)
                        (t #\|)))))))
    ;;Draw cell corners
    (dotimes (r rows)
      (dotimes (c cols)
       ; (setf (aref buffer (* *cell-height* r) (* *cell-width* c)) #\+)
        ))
    ;;Print out the maze row-by-row
    (dotimes (row out-rows)
      (let ((line (coerce (row-major-collect buffer row) 'string)))
        (format t "~a~%" line)))))

(defun row-major-collect (buffer row)
  "Collects all characters in a given row from BUFFER."
  (let* ((cols (second (array-dimensions buffer)))
         (chars (make-array cols :element-type 'character)))
    (dotimes (col cols)
      (setf (aref chars col) (aref buffer row col)))
    chars))

(render-maze (binary-tree-maze))
