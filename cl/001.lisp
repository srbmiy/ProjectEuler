;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Euler, Problem 001
;; Multiples of 3 and 5
;; answer: 234168
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro sum-iter (n val count)
  (cond ((= n count) val)
	((= 0 (mod count 3)) `(sum-iter ,n ,(+ count val) ,(+ 1 count)))
	((= 0 (mod count 5)) `(sum-iter ,n ,(+ count val) ,(+ 1 count)))
        (t `(sum-iter ,n ,val ,(+ 1 count)))))

(defparameter *res* (sum-iter 10000001 0 0))
(format t "~d~%" *res*)

