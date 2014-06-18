;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;              Problem 1
;

;; http://projecteuler.net/index.php?section=problems&id=1
;;
;; If we list all the natural numbers below 10 that are multiples of 3 or 5,
;; we get 3, 5, 6 and 9. The sum of these multiples is 23.
;;
;; Find the sum of all the multiples of 3 or 5 below 1000.


;; http://odz.sakura.ne.jp/projecteuler/index.php?Problem%201
;;
;; 10未満の自然数のうち、3 もしくは 5 の倍数になっているものは
;; 3, 5, 6, 9 の4つがあり、これらの合計は 23 になる。
;;
;; 同じようにして、1,000 未満の 3 か 5 の倍数になっている数字の合計を求めよ。




(defun demo (num &optional (count 0))
  (cond  ((= count num) 0)
         ((or (zerop (mod count 3)) (zerop (mod count 5)))
          (+ count (demo num (1+ count))))
         (t (demo num (1+ count)))))

;;==> 233168



(defun iota (m &optional (n 0) (step 1))
  (if (zerop m)
    nil
    (cons n (iota (1- m) (+ n step) step))))

(iota 10) ;==> (0 1 2 3 4 5 6 7 8 9)


(apply #'+ (remove-if-not
             #'(lambda (x)
                 (or (zerop (mod x 3)) (zerop (mod x 5))))
             (iota 10)))

;;==> 23

(apply #'+ (remove-if-not
             #'(lambda (x)
                 (or (zerop (mod x 3)) (zerop (mod x 5))))
             (iota 1000)))

;;==> 233168
