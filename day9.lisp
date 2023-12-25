(defun predict-and-sum ()
  (let ((fileexpr (mapcar #'(lambda (x)
              (read-from-string (format nil "(~a)" x))) (uiop:read-file-lines "day9.txt"))))
    (loop for x in fileexpr
          sum (anticipate x))
))

(defun anticipate (lst &optional start-at-end)
  "Recursive function - Attempt to predict the previous element of a list, following the form described by day 9
   I would make it general but I'm bad at macros"
    (labels ((rec (lst acc multiplier)
(null lst)
             (cond ((null lst)
                    return-from test2 nil)
                   ((every #'zerop lst)
                    acc)
                   (t 
                    (rec (map 'list #'- (rest lst) lst) (+ (* multiplier (first lst)) acc) (- multiplier))))))
    (rec lst 0 1)))
