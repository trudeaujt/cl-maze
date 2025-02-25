(load "~/code/maze/maze.lisp")
(load "~/code/maze/utils.lisp")

(defun binary-tree-maze (&optional (rows 15) (cols 15))
  (let ((g (make-instance 'maze-grid)))
    (initialize g :rows rows :cols cols)
    (map-vector (lambda (cell)
                  (let ((target (sample (remove nil (list (north cell)
                                                          (east cell))))))
                    (when target
                      (link cell :to target))))
                (grid g))
    g))

(binary-tree-maze 5 5)
