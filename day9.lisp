(defun mappend (fn lst)
  "See paip"
  ((apply #'append (mapcar fn lst))))
(* (+ 3 4) (+ 5 6))
(defun predict-and-sum ()
  (let ((delta 0) (degree nil) (fileexpr (mapcar #'(lambda (x)
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

(defmacro generate-recursive-case (funcname backwards)
  `(t (,funcname (map 'list #'- (rest lst) lst)
                 (,(if (equalp backwards t)
                       `(- (first lst))
                       `(+ (car (last lst)))
                       )
                       acc))))

(defun our-length (lst)
  (labels ((rec (lst acc)
             (if (null lst)
                 acc
                 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))
(defun difference (inp)
  (map 'list #'- inp (rest inp)))


(defun pyramid-num (r n) ;formula straight from wikipedia
  (* (/ (+ (expt n 2) n) 2) ;note that this 
     (/ (- (* n (- r 2)) (- r 5)) 3)))
