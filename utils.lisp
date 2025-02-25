(load "~/code/maze/maze.lisp")

(defparameter *cell-width* 7)
(defparameter *cell-height* 4)

(defmethod render-maze ((maze maze-grid))
  (let* ((rows (grid-rows maze))
         (cols (grid-cols maze))
         (out-rows (1+ (* rows *cell-height*)))
         (out-cols (1+ (* cols *cell-width*)))
         (buffer (make-array (list out-rows out-cols)
                             :element-type 'character
                             :initial-element #\Space)))
    ;;Draw outside walls
    (dotimes (c out-cols)
      (setf (aref buffer 0 c) #\-))
    (dotimes (r out-rows)
      (setf (aref buffer r 0) #\|))
    ;;Draw a wall or opening for each cell depending on its south/east links
    ;;Since we've drawn the north and west walls first, shift base rows 1+
    (dotimes (r rows)
      (dotimes (c cols)
        (let* ((cell (aref (grid maze) r c))
               (base-row (1+ (* r *cell-height*)))
               (base-col (1+ (* c *cell-width*))))
          ;;Draw cell south
          (dotimes (i *cell-width*)
            (setf (aref buffer (+ base-row (1- *cell-height*)) (+ base-col i)) 
                  (cond ((linkedp cell :to (south cell)) #\Space)
                        (t #\-))))
          ;;Draw cell east
          (dotimes (i *cell-height*)
            (setf (aref buffer (+ base-row i) (+ base-col (- *cell-width* 1)))
                  (cond ((linkedp cell :to (east cell)) #\Space)
                        (t #\|)))))))
    ;;Draw cell corners
    (dotimes (r (1+ rows))
      (dotimes (c (1+ cols))
        (setf (aref buffer (* *cell-height* r) (* *cell-width* c)) #\+)
        ))
    ;;Print out the maze row-by-row
    (fresh-line)
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

(defun for-each-row (fn array)
  (loop for i from 0 below (array-dimension array 0)
        do (let ((row (loop for j from 0 below (array-dimension array 1)
                            collect (aref array i j))))
             (funcall fn row))))

(defun map-vector (f array)
  (let* ((dims (array-dimensions array))
         (result (make-array dims)))
    (labels ((map-dim (indices remaining-dims)
               (if (null remaining-dims)
                   (setf (apply #'aref result (reverse indices))
                         (funcall f (apply #'aref array (reverse indices))))
                   (dotimes (i (first remaining-dims))
                     (map-dim (cons i indices) (rest remaining-dims))))))
      (map-dim '() dims))
    result))

(defun sample (items)
  (let ((len (length items)))
    (unless (= len 0)
      (elt items (random len)))))
