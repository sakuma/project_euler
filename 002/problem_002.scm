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



;; Rewrite to 090724

;; fibonacci
(define fibo
  (lambda (n)
    (let loop ((f1 1) (f2 1) (result ()))
      (if (> f2 n)
          result
          (loop f2 (+ f1 f2) (cons f2 result))))))

(fibo 10)
;;==> 89

(define (problem_002)
  (define (fibo n)
    (let loop ((f1 1) (f2 1) (result ()))
      (if (> f2 n)
          result
          (loop f2 (+ f1 f2) (cons f2 result)))))
  (apply + (filter even? (fibo 4000000))))

(problem_002)
  

(define problem_002
  (lambda (upper)
    (letrec ((fibo
              (lambda (n)
                (let loop ((count n) (f1 0) (f2 1))
                  (if (zero? count)
                      f2
                      (loop (- count 1) f2 (+ f1 f2)))))))
      (let loop ((count 1) (ans ()))
        (let ((fibo_num (fibo count)))
          (cond [(> fibo_num upper)
                 (apply + ans)]
                [(even? fibo_num)
                 (loop (+ count 1) (cons fibo_num ans))]
                [else (loop (+ count 1) ans)]))))))

(problem_002 4000000)
;;==> 4613732
;; Answer is correct







;; フィボナッチ数

;; 再帰版
(define fibo
  (lambda (x)
	(cond ((or (= x 0) (= x 1)) 1)
		  (else (+ (fibo (- x 1)) (fibo (- x 2)))))))

(fibo 1)
;;==> 1
(fibo 2)
;;==> 2
(fibo 3)
;;==> 3
(fibo 10)
;;==> 89


;; 末尾再帰版

(define (fibo x)
  (define (fibo-iter a b count)
	(if (= count 0)
		b
		(fibo-iter (+ a b) a (- count 1))))
  (fibo-iter 1 1 x))


(fibo 1)
;;==> 1
(fibo 2)
;;==> 2
(fibo 3)
;;==> 3
(fibo 10)
;;==> 89

;; named let
(define fibo
  (lambda (x)
	(let loop ((f1 1) (f2 1) (count x))
	  (if (= count 0)
		  f1
		  (loop f2 (+ f1 f2) (- count 1))))))
(fibo 1)
;;==> 1
(fibo 2)
;;==> 2
(fibo 3)
;;==> 3
(fibo 10)
;;==> 89


;; Make list of fibonacci

(define fibo-list
  (lambda (x)
	(cond ((zero? x) '())
		  ((= x 1) '(1))
		  (else
		   (cons (fibo x)
				 (fibo-list (- x 1)))))))

(fibo-list 10)
;;==> (89 55 34 21 13 8 5 3 2 1)



;; 指定数値未満のフィボナッチ数列を算出

(define fibo-list-upper
  (lambda (x)
	(let loop ((upper x) (ans 0) (count 0))
	  (if (< upper (fibo count))
		  '()
		  (cons (fibo count)
				(loop x (fibo count) (+ count 1)))))))


(fibo-list-upper 5)
;;==> (1 1 2 3 5)
(fibo-list-upper 20)
;;==> (1 1 2 3 5 8 13)
(fibo-list-upper 100)
;;==> (1 1 2 3 5 8 13 21 34 55 89)
(fibo-list-upper 1000)
;;==> (1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987)


(define sum
  (lambda (ls)
	(if (null? ls)
		0
		(+ (car ls) (sum (cdr ls))))))

(sum (fibo-list-upper 100))
;;==> 232

(define even-list
  (lambda (ls)
	(cond ((null? ls) '())
		  ((even? (car ls))
		   (cons (car ls) (even-list (cdr ls))))
		  (else (even-list (cdr ls))))))

(even-list (fibo-list-upper 10))
;;==> (2 8)
(even-list (fibo-list-upper 200))
;;==> (2 8 34 144)
(even-list (fibo-list-upper 1000))
;;==> (2 8 34 144 610)
(even-list (fibo-list-upper 4000000))
;;==> (2 8 34 144 610 2584 10946 46368 196418 832040 3524578)


;; 後は全てを足すだけ

(sum (even-list (fibo-list-upper 4000000)))
;;==> 4613732

;; Final answer!!



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    ;;
;;         More beautifully           ;;
;;                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Rewrite fibo
(define fibo
  (lambda (x)
	(let loop ((a 1) (b 0) (result 0))
	  (if (<= x result)
		  a
		  (loop (+ a b) a (+ result 1))))))

(fibo 1)
;;==> 1
(fibo 2)
;;==> 2
(fibo 3)
;;==> 3
(fibo 10)
;;==> 89

;; Make list of fibonacci

(define fibo-list
  (lambda (x)
	(let loop ((a 1) (b 0) (count 0) (result ()))
	  (if (<= x count)
		  result
		  (loop (+ a b) a (+ count 1) (cons a result))))))

(fibo-list 5)
;;==> (5 3 2 1 1)
(fibo-list 10)
;;==>  (55 34 21 13 8 5 3 2 1 1)


;; Upper case

(define fibo-list-upper
  (lambda (x)
	(let loop ((a 1) (b 0) (result ()))
	  (if (<= x a)
		  result
		  (loop (+ a b) a (cons a result))))))


(fibo-list-upper 10)
;;==> (8 5 3 2 1 1)
(fibo-list-upper 100)
;;==> (89 55 34 21 13 8 5 3 2 1 1)


;; フィボナッチ数列の偶数項のみを"直接"リスト化

(define fibo-list-upper-even
  (lambda (x)
	(let loop ((a 1) (b 0) (result ()))
	  (cond ((<= x a) result)
			((= (modulo a 2) 0) ; or ... (even? a)
			 (loop (+ a b) a (cons a result)))
			(else (loop (+ a b) a result))))))
  
;; フィボナッチ数列をリスト化してから、偶数値の項を選出

(define fibo-list-upper-even
  (lambda (x)
	(define lists
	  (lambda ()
		(let loop ((a 1) (b 0) (result ()))
		  (if (<= x a)
			  result
			  (loop (+ a b) a (cons a result))))))
	(filter even? (lists))))
;(filter (lambda (x) (= (modulo x 2) 0)) (lists))))

(fibo-list-upper-even 10)
;;==> (8 2)
(fibo-list-upper-even 100)
;;==> (34 8 2)


;; Rewrite letrec

(define fibo-list-upper-even
  (lambda (x)
	(letrec ((lists 
			  (lambda (x)
				(let loop ((a 1) (b 0) (result ()))
				  (if (<= x a)
					  result
					  (loop (+ a b) a (cons a result)))))))
	  (filter even? (lists x)))))

(fibo-list-upper-even 10)
;;==> (8 2)
(fibo-list-upper-even 100)
;;==> (34 8 2)


;; 最後に得られたリストの総和を算出
;; applyを追加して終了

(define sum-of-fibo-upper-even-list
  (lambda (x)
	(letrec ((lists 
			  (lambda (x)
				(let loop ((a 1) (b 0) (result ()))
				  (if (<= x a)
					  result
					  (loop (+ a b) a (cons a result)))))))
	  (apply + (filter even? (lists x))))))


(sum-of-fibo-upper-even-list 100)
;;==> 44

(sum-of-fibo-upper-even-list 4000000)
;;==> 4613732
;; Final answer!!






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    ;;
;;           反復的プロセス             ;;
;;             (末尾再帰)              ;;
;;                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))


(define (fib n)
  (define (fib-iter a b counter)
	(if (> counter n)
		b
		(fib-iter (+ a b) a (+ counter 1))))
  (fib-iter 1 0 1))

(fib 400000)














;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;         他者解答
;


(define initial_list (list 9 8 7 6 5 4 3 2 1))
(define prev_1
	(lambda (l)
		(car l)))
(define prev_2
	(lambda (l)
		(cadr l)))
(define next_number
	(lambda (l)
		(+ (prev_1 l) (prev_2 l))))
(define build_list
	(lambda (l)
		(if (> (next_number l) 4000000)
			l
			(build_list (cons (next_number l) l)))))
(define fib_list (build_list initial_list))
(define result
	(lambda (l sum)
		(cond
			((null? l) sum)
			((even? (car l)) (result (cdr l) (+ sum (car l))))
			(else (result (cdr l) sum)))))

(display (result fib_list 0))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use srfi-1)
(define (solve)
  (define fibos
    (let gen-fibos-loop ((fibos '(1 1)))
      (define next-val (+ (first fibos) (second fibos)))
      (if (> next-val 4000000) fibos
          (gen-fibos-loop (cons next-val fibos)))))
  (apply + (filter even? fibos)))

(solve)
;;==> 4613732

;; excellent!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define problem-002
  (lambda (n)
	(define fib-list
      (lambda ()
        (let loop ([a 1] [b 1] [result '()])
          (if (>= a n)
              result
              (loop (+ a b) a (cons a result))))))
	(apply + (filter even? (fib-list)))))
 
(problem-002 4000000)
;;==> 4613732

;; I like it!


(use srfi-1)
(define problem-002_1
  (lambda (n)
	(define fib-list
      (lambda ()
        (let loop ([a 1] [b 1] [result '()])
          (if (>= a n)
              result
              (loop (+ a b) a (cons a result))))))
	(filter even? (fib-list))))

(problem-002_1 4000000)
;;==> (3524578 832040 196418 46368 10946 2584 610 144 34 8 2)

(define problem-002_2
  (lambda (n)
	(define fib-list
      (lambda ()
        (let loop ([a 1] [b 1] [result '()])
          (if (>= a n)
              result
              (loop (+ a b) a (cons a result))))))
	(fib-list)))


(problem-002_2 10)
;;==> (8 5 3 2 1)

(problem-002_2 100)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrong!!!
;; 4百万項の偶数値の合計だと勘違いパターン

(define pickup-fibo
  (lambda (ls)
	(cond ((null? ls) '())
		  ((even? (car ls))
		   (cons (car ls) (pickup-fibo (cdr ls))))
		  (else (pickup-fibo (cdr ls))))))

(pickup-fibo (fibo-list 10))

;; 偶数の項をpickup
(define pickup-fibo
  (lambda (x)
	(cond ((zero? x) '())
		  ((= (modulo x 2) 0)
		   (cons (fibo x)
				 (pickup-fibo (- x 1))))
		  (else (pickup-fibo (- x 1))))))

(pickup-fibo 10)
;;==> (89 34 13 5 2)

(define sum-of-pickup-fibo
  (lambda (x)
	(cond ((zero? x) 0)
		  ((= (modulo x 2) 0)
		   (+ (fibo x)
			  (sum-of-pickup-fibo (- x 1))))
		  (else (sum-of-pickup-fibo (- x 1))))))

(sum-of-pickup-fibo 10)
;;==> 143

;;検算
;; (89 34 13 5 2) の総和

(define sum
  (lambda (ls)
	(if (null? ls)
		0
		(+ (car ls) (sum (cdr ls))))))

(sum '(89 34 13 5 2))
;;==> 143
;; Correct

;; But!!!
;;総和を求める式も末尾再帰で表現しないと計算が膨大になる。

;; 1万項
(time (sum-of-pickup-fibo 10000))

;;(time (sum-of-pickup-fibo 10000))
;; real  22.875
;; user  21.630
;; sys    0.310

;; 1万項の計算で22秒もかかるのでは、400万項は時間がかかりすぎる。

(define sum-of-pickup-fibo2
  (lambda (x)
	(cond ((zero? x) 0)
		  ((= (modulo x 2) 0)
		   (+ (fibo x)
			  (sum-of-pickup-fibo (- x 2))))
		  (else (sum-of-pickup-fibo (- x 2))))))

(time (sum-of-pickup-fibo2 10000))


(define (sum-of-fibo n)
  (define (sum-of-fibo-iter n ans)
	(cond ((= n 0) ans)
		  ((even? n)
		   (sum-of-fibo-iter (- n 1) (+ (fibo n) ans)))
		  (else (sum-of-fibo-iter (- n 1) (+ ans 0)))))
  (sum-of-fibo-iter n 0))

(sum-of-fibo 10)
;;==> 143

(trace sum-of-fibo)


(time (sum-of-fibo 1000))
;(time (sum-of-fibo 1000))
; real   0.044
; user   0.040
; sys    0.000
(time (sum-of-fibo 10000))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:




;;;
;;  Step 1 - print fibonacci-number
;
---------------------------
;;厳密な場合
(define (fib3 n)
  (if (= n 0)
	  0
	  (let loop ((m 1) (x 0) (y 1) (z 1))
		(if (= m n)
			y
			(let ((u y) (v z))
			  (loop (+ m 1) u v (+ u v)))))))

(fib3 10)
;;==> 55

(define (fib4 n)
  (if (= n 0)
	  0
	  (let loop ((m 1) (y 1) (z 1))
		(if (= m n)
			y
			(let ((u y) (v z))
			  (loop (+ m 1) v (+ u v)))))))

(fib4 10)



;;該当問題の場合
(define (fib5 n)
  (if (= n 0)
	  0
	  (let loop ((m 1) (y 1) (z 2)) ;; z->2
		(if (= m n)
			y
			(let ((u y) (v z))
			  (loop (+ m 1) v (+ u v)))))))

(fib5 10)
;;==> 89

---------------------

(define (fibo n)
  (letrec ((iter (lambda (m y z)
				   (if (= m n)
					   y
					   (let ((u y) (v z))
						 (iter (+ m 1) v (+ u v)))))))
	(iter 0 1 1)))

(fibo 10)

---------------------
;;;
;;  Step 2 - print fibonacci-number for 1 to 10
;
(use srfi-1)
(letrec ((num
		  (lambda (lis)
			(if (null? lis)
				'done
				(fold
				 (lambda (a b)
				   (print (fib3 a)))
				 (car lis) (cdr lis))))))
  (num (iota 10 1)))

;;
;; Another way  ( fold -> for-each )
;;
(use srfi-1)
(letrec ((fibo-num
		  (lambda (lis)
			(for-each
			 (lambda (x)
			   (print (fibo x))) lis))))
  (fibo-num (iota 10 1)))


;	(num '( 2 4 6 8 10)))


;;;
;;  Step 3 
;      Print fibonacci-number of Even-valued for 1 to n
(letrec ((num
		  (lambda (lis)
			(if (null? lis)
				'done
				(fold
				 (lambda (a b)
				   (if (odd? a)
					   (print (fib3 a))))
				 (car lis) (cdr lis))))))
  (num (iota 10 1)))


;;;
;; Final
;     


(define (fibo n)
  (letrec ((iter (lambda (m y z)
				   (if (= m n)
					   y
					   (let ((u y) (v z))
						 (iter (+ m 1) v (+ u v)))))))
	(iter 0 1 1)))


(letrec ((num
		  (lambda (lis)
			(cond [(null? lis) 0]
				  [(even? (car lis)) (+ (fibo (car lis)) (num (cdr lis)))]
				  [else (num (cdr lis))]))))
  (num (iota 10 1)))

(use srfi-1)
(time (letrec ((num
		  (lambda (lis)
			(cond [(null? lis) 0]
				  [(even? (car lis)) (+ (fibo (car lis)) (num (cdr lis)))]
				  [else (num (cdr lis))]))))
  (num (iota 10000 1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



-----------------
(define (ev lis)
	(even? (car lis)))

(even? 2)
(ev '(1 2))
--------------------------------------------------
;;;   
;;  Step 3   x bad answer
;
;      Print fibonacci-number of Even number for 1 to n

(letrec ((num
		  (lambda (lis)
			(if (null? lis)
				'done
				(fold
				 (lambda (a b)
				   (if (even? (fib3 a))
					   (print (fib3 a))))
				 (car lis) (cdr lis))))))
  (num (iota 15 1)))

----------------------------------------------------
;;;
;; Step 4
;    Print total of fibonacci-number of Even number for 1 to n


(letrec ((even-num
		  (lambda (x)
			(cond [(= x 0) 0]
				  [(even? (fib3 x)) (fib3 x)]
				  [else 0])))
		 (sum
		  (lambda (lis)
			(cond [(null? lis) 0]
				  [(> (even-num (car lis)) 0)
				   (+ (even-num (car lis)) (sum (cdr lis)))]
				  [else (sum (cdr lis))]
				  ))))
  (sum (iota 10 1)))

(use srfi-1)
(time (letrec ((sum
		  (lambda (lis)
			(cond [(null? lis) 0]
				  [(even? (car lis)) (+ (fib3 (car lis)) (sum (cdr lis)))]
				  [else (sum (cdr lis))]))))
  (sum (iota 10 1))))

(iota 100)

(even? 2)

*************   Fibonacci   ****************

(use srfi-1)
(define (fib3 n)
  (let loop ((m 1) (x 0) (y 1) (z 1))
	(if (= m n)
		y
		(let ((u y) (v z))
		  (loop (+ m 1) u v (+ u v))))))

********************************************

*************  Fibonacci 2  **************

(define (fibo n)
  (letrec ((iter (lambda (m x y z)
				   (if (= m n)
					   y
					   (let ((u y) (v z))
						 (iter (+ m 1) u v (+ u v)))))))
	(iter 0 0 1 1)))

********************************************













