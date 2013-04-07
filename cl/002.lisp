;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Euler, Problem 002
;; Even Fibonacci numbers
;; answer: 4613732
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro fib-sum (fst snd val bound)
  (let ((next-fst snd)
	(next-snd (+ fst snd)))
    (cond ((> snd bound) val)
	  ((= 0 (mod snd 2)) `(fib-sum ,next-fst ,next-snd ,(+ val snd) ,bound))
	  (t `(fib-sum ,next-fst ,next-snd ,val ,bound)))))

(defparameter *res* (fib-sum 1 2 0 4000000))
(format t "Answer: ~d~%" *res*)