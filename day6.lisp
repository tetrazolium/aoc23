(defvar test '((7 9) (15 40) (30 200)))
(defvar racerecords '((48 296) (93 1928) (85 1236) (95 1391)))
(defun num-possible-wins (time dist)
  (- (- (ceiling (+ (/ time 2) (/ (sqrt (- (expt time 2) (* 4 dist))) 2)))
        (floor (- (/ time 2) (/ (sqrt (- (expt time 2) (* 4 dist))) 2))))
     1))
(reduce #'* (mapcar #'(lambda (x) (num-possible-wins (first x) (second x))) racerecords))
(defun element-sum (lists)
  (reduce #'(lambda (l1 l2) (mapcar #'+ l1 l2)) lists))
(defun element-concat (lists)
  (reduce #'(lambda (l1 l2) 
              (mapcar #'(lambda (i1 i2) (parse-integer (format nil "~a~a" i1 i2)))
                      l1 l2))
          lists))
(num-possible-wins (first (element-concat test)) (second (element-concat test)))
(num-possible-wins (first (element-concat racerecords)) (second (element-concat racerecords)))
