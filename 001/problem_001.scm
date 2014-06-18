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



;; Rewrite to 090724
(use srfi-1)
(apply + (filter (lambda (x)
                   (or (zero? (modulo x 3))
                       (zero? (modulo x 5))))
                 (iota 1000)))
;;That's it!!


;; １０未満
;;==> 23

;; 1,000未満
;;==> 233168
;; answer is correct



(define pick
  (lambda (x)
    (cond ((zero? x) '())
          ((zero? (modulo x 5))
           (cons x (pick (- x 1))))
          ((zero? (modulo x 3))
           (cons x (pick (- x 1))))
          (else (pick (- x 1))))))
(pick 10)
;;==> (10 9 6 5 3)

(define pick2
  (lambda (x)
    (let ((value (- x 1)))
      (cond ((zero? value) '())
            ((or (zero? (modulo value 5)) (zero? (modulo value 3)))
             (cons value (pick (- value 1))))
            (else (pick (- value 1)))))))
(pick2 10)
;;==> (10 9 6 5 3)

(use srfi-1)
(define pick3
  (lambda (x)
    (letrec ((lists
               (lambda (x)
                 (let ((value (- x 1)))
                   (cond ((zero? value) '())
                         ((or (zero? (modulo value 5)) (zero? (modulo value 3)))
                          (cons value (pick (- value 1))))
                         (else (pick (- value 1))))))))
      (apply + (lists x)))))

(pick3 10)
;;==> 23
(pick3 1000)
;;==> 233168



(use srfi-1)
(define (pick lis)
  (if (null? lis)
    "Done"
    (fold
      (lambda (a b)
        (cond ((= (modulo a 5) 0) (print a))
              ((= (modulo a 3) 0) (print a))))
      (car lis) (cdr lis))))

(pick (iota 10))

;; 別解1

(define (pick2 lis)
  (fold
    (lambda (a b)
      (cond ((= (modulo a 5) 0) (print a))
            ((= (modulo a 3) 0) (print a))))
    (car lis) (cdr lis)))

(pick2 (iota 10))

;; 別解2

(letrec ((num
           (lambda (lis)
             (if (null? lis)
               "Done"
               (fold
                 (lambda (a b)
                   (cond ((= (modulo a 5) 0) (print a))
                         ((= (modulo a 3) 0) (print a))))
                 (car lis) (cdr lis))))))
  (num (iota 10)))

;; 別解3


(letrec ((num
           (lambda (lis)
             (fold
               (lambda (a b)
                 (cond ((= (modulo a 5) 0) (print a))
                       ((= (modulo a 3) 0) (print a))))
               (car lis) (cdr lis)))))
  (num (iota 10)))


(letrec ((sum
           (lambda (lis)
             (cond [(null? lis) 0]
                   [(= (modulo (car lis) 5) 0) (+ (car lis) (sum (cdr lis)))]
                   [(= (modulo (car lis) 3) 0) (+ (car lis) (sum (cdr lis)))]
                   [else (sum (cdr lis))]
                   ))))
  (sum (iota 1000)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; まずは普通に10未満の合計

(define sum
  (lambda (ls)
    (cond ((null? ls) 0)
          (else
            (+ (car ls) (sum (cdr ls)))))))
(use srfi-1)
(sum (iota 10))
;;==> 45

(define pickup
  (lambda (ls)
    (cond ((null? ls) '())
          ((= (car ls) 0) (pickup (cdr ls)))
          ((= (modulo (car ls) 5) 0)
           (cons (car ls) (pickup (cdr ls))))
          ((= (modulo (car ls) 3) 0)
           (cons (car ls) (pickup (cdr ls))))
          (else (pickup (cdr ls))))))

(pickup (iota 10))
;;==> (3 5 6 9)

;; sumとpickupをあわせる。

(define pickup-sum
  (lambda (ls)
    (cond ((null? ls) 0)
          ((= (modulo (car ls) 5) 0)
           (+ (car ls) (pickup-sum (cdr ls))))
          ((= (modulo (car ls) 3) 0)
           (+ (car ls) (pickup-sum (cdr ls))))
          (else (pickup-sum (cdr ls))))))

(pickup-sum (iota 10))
;;==> 23
(pickup-sum (iota 1000))
;;==> 233168


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;     Problem 1
;;;
;;    １０未満の自然数のうち、３もしくは５の倍数になっているものは
;     ３、５、６、９の４つがあり、これらの合計は２３になる
;     同様に１,０００未満の３か５の倍数の和を求めよ。
;

;;
;; Step 1
;;１０未満の３、５の倍数の表示
;;

;;
;; No.1
;;

(define (pick lis)
  (if (null? lis)
    'Done
    (fold
      (lambda (a b)
        (cond ((= (modulo a 5) 0) (print a) )
              ((= (modulo a 3) 0) (print a) )))
      (car lis) (cdr lis))))

(pick (iota 10 1))

;;
;; No.2
;;
(define (pick2 lis)
  (define (conv a b)
    (cond [(= 0 (modulo a 5)) (print a)]
          [(= 0 (modulo a 3)) (print a)]))
  (fold conv (car lis) (cdr lis)))

(pick2 (iota 10))

;;
;; No.3
;;
(define (pick3 lis)
  (for-each (lambda (a)
              (cond [(= 0 (modulo a 5)) (print a)]
                    [(= 0 (modulo a 3)) (print a)]))
            (cdr lis)))

(pick3 (iota 100 1))



;;(pick (iota n)) ; n:数字


;;
;; 補足 iota
;;
;; (iota 10) と (list-tabulate 10 values) は等価
;;

;
; 別解 (直接計算)
;
(letrec  ((num (lambda (lis)
                (if (null? lis)
                  'Done
                  (fold
                    (lambda (a b)
                      (cond [(= (modulo a 5) 0) (print a)]
                            [(= (modulo a 3) 0) (print a)]))
                    (car lis) (cdr lis))))))
        (num (iota 10)))


;;
;; Step 2
;;  １０未満の３、５の倍数の和
;;

(use srfi-1)
(letrec ((sum (lambda (lis)
                (cond [(null? lis) 0]
                      [(= 0 (modulo (car lis) 5)) (+ (car lis) (sum (cdr lis)))]
                      [(= 0 (modulo (car lis) 3)) (+ (car lis) (sum (cdr lis)))]
                      [else (sum (cdr lis))]))))
  (sum (iota 10)))

;;
;; Final step
;;

(sum (iota 1000)) ;; 数字を変化させれば求められる

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
