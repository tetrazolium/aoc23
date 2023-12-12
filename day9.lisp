(defun mappend (fn lst)
  "See paip"
  ((apply #'append (mapcar fn lst))))
(* (+ 3 4) (+ 5 6))
(defun predict ()
  (let ((delta 0) (degree nil) (fileexpr (mapcar #'(lambda (x)
              (read-from-string (format nil "(~a)" x))) (uiop:read-file-lines "day9example.txt"))))
    (loop for x in fileexpr do
          (print x)
          (print (test2 x)))
))

(defun test2 (lst)
  "Recursive function - TCO function to get the degree of a sequence"
  (let ((base 0))
    (values (labels ((rec (lst acc)
(null lst)
             (cond ((null lst)
                    return-from test2 nil)
                   ((every #'zerop lst)
                    acc)
                   (t (setf base (first lst))
                    (rec (map 'list #'- lst (rest lst)) (1+ acc))))))
    (- (rec lst 0) 1))
            (- base))))

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
