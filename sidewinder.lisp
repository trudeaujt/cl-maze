
(load "~/code/maze/maze.lisp")
(load "~/code/maze/utils.lisp")

(defun sidewinder-maze (&optional (rows 4) (cols 4))
  (let ((g (make-instance 'maze-grid)))
    (initialize g :rows rows :cols cols)
    (for-each-row (lambda (row) 
                    (let ((run))
                      (mapc (lambda (cell)
                              (let* ((at-east-bound (null (east cell)))
                                    (at-north-bound (null (north cell)))
                                    (should-close-out (or at-east-bound
                                                          (and (not at-north-bound)
                                                               (= 0 (random 2))))))
                                (push cell run)
                                (if should-close-out
                                    (let ((choice (sample run)))
                                      (when (north choice) (link choice :to (north choice)))
                                      (setf run '()))
                                    (link cell :to (east cell)))
                                )) 
                        row))) 
                  (grid g))
    g))

(sidewinder-maze 15 15)
