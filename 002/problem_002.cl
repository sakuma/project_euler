;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;              Problem 2
;;;

;; http://projecteuler.net/index.php?section=problems&id=2
;;
;; Each new term in the Fibonacci sequence is generated
;; by adding the previous two terms.
;; By starting with 1 and 2, the first 10 terms will be:
;;
;; 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
;;
;; Find the sum of all the even-valued terms in the sequence
;; which do not exceed four million.


;; http://odz.sakura.ne.jp/projecteuler/index.php?Problem%202
;; フィボナッチ数列の項は前の2つの項の和である。
;; 最初の2項を 1, 2 とすれば、最初の10項は以下の通りである。
;;
;;  1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
;;
;; 数列の項が400万を超えない範囲で、偶数の項の総和を求めよ。
;;



;; (defun fib (num)
;;   (if (< num 4)
;; 	  num
;; 	  (+ (fib (- num 1))
;; 		 (fib (- num 2)))))

(defun fib (num &optional (a 0) (b 1))
	(if (zerop num)
		a
		(fib (1- num) b (+ a b))))


(defun solve (&optional (count 1) (result 0))
  (let ((comp (fib count)))
	(cond ((> comp 4000000) result)
		  ((evenp comp)
		   (solve (1+ count) (+ comp result)))
		  (t (solve (1+ count) result)))))
			
;;==> 4613732






