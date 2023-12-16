(setf *read-eval* nil)
(defun load-and-calc-file ()
  (let ((fileexpr
          (uiop:read-file-string "day15.txt")))
    (reduce #'+ (uiop:split-string (remove #\linefeed (remove #\return fileexpr)) :separator ",") :key #'hash-algo)))
(defun hash-algo (str)
  (let ((acc 0))
   (loop for piece across str do
        (setf acc (rem (* (+ acc (char-code piece)) 17) 256)))
   acc))
