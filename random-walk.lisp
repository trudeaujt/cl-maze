(load "~/code/maze/maze.lisp")

(defun aldous-broder (&optional (rows 4) (cols 4))
  (let ((g (make-instance 'maze-grid)))
    (initialize g :rows rows :cols cols)
    (let ((cell (sample-grid g))
          (unvisited (1- (* (grid-rows g) (grid-cols g)))))
      (loop while (< 0 unvisited)
            do (let ((neighbor (sample-neighbor cell)))
                 (when (null (links neighbor))
                   (progn (link cell :to neighbor) 
                          (decf unvisited)))
                 (setf cell neighbor))))
    (calculate-distances g)
    g))

(aldous-broder 12 12)

(defun wilsons (&optional (rows 4) (cols 4))
  (let ((g (make-instance 'maze-grid)))
    (initialize g :rows rows :cols cols)
    (let ((unvisited (make-array (* (grid-rows g) (grid-cols g)))))
      ;; Setup the list of unvisited cells, and then remove a random cell to become the first 'visited' cell.
      (loop for i below (grid-rows g) do
            (loop for j below (grid-cols g) do
                  (setf (aref unvisited (+ (* i cols) j))
                        (aref (grid g) i j))))
      (setf unvisited (delete (sample unvisited) unvisited))
      (loop while (< 0 (length unvisited))
            do (let* ((cell (sample unvisited))
                      (path (list cell)))
                 ;; Walk randomly through the maze until we hit a visited cell.
                 ;; Along the way, detect loops in the path and remove them.
                 (loop while (find cell unvisited)
                       do (setf cell (sample-neighbor cell)
                          (when (position cell path)
                            (setf path (subseq path 0 (1+ (position cell path)))))
                          (push cell (cdr (last path)))))
                 ;; After we've reached a visited cell, carve a path and remove it from the unvisited list.
                 (dolist (cell path)
                   (loop for cell on path
                         for neighbor = (second cell)
                         while neighbor
                         do (link (first cell) :to neighbor)
                            (setf unvisited (delete (first cell) unvisited)))))))
    (calculate-distances g)
    g))

(wilsons 12 12)
