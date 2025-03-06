(load "utils.lisp")

;;;====;;;
;;;Cell;;;
;;;====;;;
(defclass cell ()
  ;;A representation of an individual cell. Links to other cells to form passageways.
  ((row    :accessor cell-row   :initform nil)
   (col    :accessor cell-col   :initform nil)
   (clinks :accessor cell-links :initform (make-hash-table :test 'equal))
   (north  :accessor north      :initform nil)
   (south  :accessor south      :initform nil)
   (east   :accessor east       :initform nil)
   (west   :accessor west       :initform nil)))

(defmethod print-object ((cell cell) out)
  (print-unreadable-object (cell out :type t)
    (format out "@~A,~A" (cell-row cell) (cell-col cell))))

(defmethod initialize ((cell cell) &key row col)
  (setf (cell-row cell) row)
  (setf (cell-col cell) col)
  cell)

(defmethod link ((cell cell) &key to (bi t))
  (setf (gethash to (cell-links cell)) to)
  (when bi (link to :to cell :bi nil))
  cell)

(defmethod unlink ((cell cell) &key from (bi t))
  (remhash from (cell-links cell))
  (when bi (unlink from :from cell :bi nil))
  cell)

(defmethod links ((cell cell))
  (loop for link being the hash-keys of (cell-links cell)
        collect link))

(defmethod linkedp ((cell cell) &key to)
  (gethash to (cell-links cell)))

(defmethod neighbors ((cell cell))
  (remove nil (list (north cell)
                    (south cell)
                    (east  cell)
                    (west  cell))))

(defmethod distances ((cell cell))
  ;;Traverse the maze calculating the distances from a cell.
  (let ((distances (make-instance 'path))
        (frontier (list cell)))
    (init-path distances cell)
    (loop while frontier
          do (let ((new-frontier '()))
                (dolist (cell frontier)
                  (dolist (linked (links cell))
                    (unless (get-dist distances linked)
                      (set-dist distances linked 
                                (1+ (get-dist distances cell)))
                      (push linked new-frontier))))
                (setf frontier new-frontier)))
    distances))

;;;====;;;
;;;Maze;;;
;;;====;;;
(defclass maze-grid ()
  ;;A representation of the grid on which cells live.
  ((rows :accessor grid-rows)
   (cols :accessor grid-cols)
   (grid :accessor grid)
   (dist :accessor dist)))

(defmethod print-object ((g maze-grid) out)
  (print-unreadable-object (g out :type t)
    (render-maze g)))

(defmethod initialize ((g maze-grid) &key rows cols)
  (setf (grid-rows g) rows)
  (setf (grid-cols g) cols)
  (setf (grid g) (make-array (list rows cols) :initial-element nil))
  (place-cells g)
  (configure-cells g)
  g)

(defmethod place-cells ((g maze-grid))
  (dotimes (i (grid-rows g))
    (dotimes (j (grid-cols g))
      (setf (aref (grid g) i j) (make-instance 'cell)))))

(defmethod configure-cells ((g maze-grid))
  ;;Places cells in the grid and sets their neighbors.
  (defun neighbor-at (row col)
    (when (and (>= row 0)
               (>= col 0)
               (<  row (grid-rows g))
               (<  col (grid-cols g)))
      (aref (grid g) row col)))
  (dotimes (row (grid-rows g))
    (dotimes (col (grid-cols g))
      (let ((cell (aref (grid g) row col)))
        (initialize cell :row row :col col)
        (setf (north cell) (neighbor-at (1- row) col))
        (setf (south cell) (neighbor-at (1+ row) col))
        (setf (east  cell) (neighbor-at row (1+ col)))
        (setf (west  cell) (neighbor-at row (1- col)))))))

(defmethod calculate-distances ((g maze-grid))
  (let ((path (make-instance 'path))
        (root (aref (grid g) 0 0)))
    (init-path path root)
    (setf (dist g) (distances root))))

(defparameter *cell-width* 8)
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
               (base-col (1+ (* c *cell-width*)))
               (dist (dist maze)))
          ;;Draw distances
          (setf (aref buffer 
                      (+ base-row (floor *cell-height* 3))
                      (+ base-col (ceiling *cell-width* 3)))
                (base62-char (get-dist dist cell)))
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
        (setf (aref buffer (* *cell-height* r) (* *cell-width* c)) #\+)))
    ;;Print out the maze row-by-row
    (fresh-line)
    (dotimes (row out-rows)
      (let ((line (coerce (row-major-collect buffer row) 'string)))
        (format t "~a~%" line)))))

;;;===========;;;
;;;Pathfinding;;;
;;;===========;;;
(defclass path ()
  ((root  :accessor root)
   (cells :accessor cells :initform (make-hash-table :test #'equal))))

(defmethod init-path ((p path) root)
  (setf (gethash root (cells p)) 0))

(defmethod get-dist ((p path) cell)
  (gethash cell (cells p)))

(defmethod set-dist ((p path) cell distance)
  (setf (gethash cell (cells p)) distance))

(defmethod get-cells-in-path ((p path))
  (loop for key being the hash-keys of (cells p)
        collect key))

(defmethod render-path ((p path))
  (maphash (lambda (key value)
             (format t "~A: ~A~%" key value))
           (cells p)))

;;;=====;;;
;;;Tests;;;
;;;=====;;;
(defun test-cell ()
  (let ((c1 (make-instance 'cell))
        (c2 (make-instance 'cell))
        (c3 (make-instance 'cell)))
    (initialize c1 :row 5 :col 1)
    (initialize c2 :row 5 :col 2)
    (initialize c3 :row 5 :col 3)
    (link c1 :bi t :to c2)
    (link c2 :bi t :to c3)
    (format t "Links for c1:~A~%Links for c2:~A~%Links for c3:~A~%" (links c1) (links c2) (links c3))
    (mapcar #'describe (links c2))))

(defun teset-path ()
  (let ((c1 (make-instance 'cell))
        (c2 (make-instance 'cell))
        (c3 (make-instance 'cell))
        (c4 (make-instance 'cell)))
    (initialize c1 :row 0 :col 0)
    (initialize c2 :row 1 :col 0)
    (initialize c3 :row 0 :col 1)
    (initialize c4 :row 0 :col 2)
    (link c1 :bi t :to c2)
    (link c1 :bi t :to c3)
    (link c4 :bi t :to c3)
    (format t "Links for c1:~A~%Links for c2:~A~%Links for c3:~A~%Links for c4:~A~%" (links c1) (links c2) (links c3) (links c4))
    (distances c1)))

(defun test-grid ()
  (let ((g (make-instance 'maze-grid)))
    (initialize g :rows 3 :cols 3)
    (describe g)
    (when t (map-vector #'describe (grid g)))
    g))

(when nil
  (progn 
    (test-cell)
    (test-grid)
    (test-djikstra)))
