;;;============;;;
;;;Cell section;;;
;;;============;;;
(defclass cell ()
  ((row    :accessor cell-row :initform nil)
   (col    :accessor cell-col :initform nil)
   (clinks :accessor cell-links)
   (north  :accessor north    :initform nil)
   (south  :accessor south    :initform nil)
   (east   :accessor east     :initform nil)
   (west   :accessor west     :initform nil)))

(defmethod print-object ((cell cell) out)
  (print-unreadable-object (cell out :type t)
    (format out "@~A,~A" (cell-row cell) (cell-col cell))))

(defmethod initialize ((cell cell) &key row col)
  (setf (cell-row cell) row)
  (setf (cell-col cell) col)
  (setf (cell-links cell) (make-hash-table :test 'equal))
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

;;;=================;;;
;;;Maze grid section;;;
;;;=================;;;
(defclass maze-grid ()
  ((rows :accessor grid-rows)
   (cols :accessor grid-cols)
   (grid :accessor grid)))

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

;;;Test
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

(defun test-grid ()
  (let ((g (make-instance 'maze-grid)))
    (initialize g :rows 3 :cols 3)
    (describe g)
    (when t (map-vector #'describe (grid g)))
    g))

(when nil
  (progn 
    (test-cell)
    (test-grid)))
