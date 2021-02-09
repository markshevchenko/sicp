#lang sicp
; Exercise 1.2
; (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

(define (square x) (* x x))

(define (sum-of-squares a b) (+ (square a) (square b)))

; Exercise 1.3
(define (sum-of-squares-of-two-greatest a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c))
        ((and (< b a) (< b c)) (sum-of-squares a c))
        (else (sum-of-squares a b))))

(define (cube x) (* x x x))

(define (average a b)
  (/ (+ a b) 2.0))

; Exercise 1.7
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) (* (abs guess) 0.0001)))

(define (sqrt1 x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (iter guess)
    (if (good-enough? guess x)
        guess
        (iter (improve guess))))
  (iter 1.0))

; Exercize 1.8
(define (cube-root1 x)
  (define (imporve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) (* (abs guess) 0.0001)))

  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (imporve guess))))

  (iter 1.0))
    

(define (inc i) (+ i 1))

(define (dec i) (- i 1))

; Exercise 1.11 (recursive)
(define (f1-11a n)
  (if (< n 3)
      n
      (+ (f1-11a (- n 1)) (f1-11a (- n 2)) (f1-11a (- n 3)))))

; Exercise 1.11 (iterative)
(define (f1-11b n)
  (define (iter a b c i)
    (if (= i 0)
        a
        (iter b c (+ a b c) (dec i))))

  (iter 0 1 2 n))

; Exercise 1.12 (Pascal's triangle)
(define (pascal y x)
  (cond ((= x 0) 1)
        ((= x y) 1)
        (else (+ (pascal (dec y) x) (pascal (dec y) (dec x))))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt2 b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* b a)))))

  (iter b n 1))

; Exercise 1.19
(define (fibonacci n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
          ((even? count) (iter a
                               b
                               (+ (square p) (square q))
                               (+ (* 2 p q) (square q))
                               (/ count 2)))
          (else (iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- count 1)))))

  (iter 1 0 0 1 n))

(define (prime? n)
  (define (divides? a b) (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (inc test-divisor)))))
    
  (= n (find-divisor n 2)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Exercise 1.29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (define y (f (+ a (* k h))))
    (cond ((= k 0) y)
          ((= k n) y)
          ((even? k) (* 2 y))
          (else (* 4 y))))

  (* h (/ 1 3.0) (sum term 0 inc n)))

; Exercise 1.30
(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))

  (iter a 0))

; Exercise 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))

  (iter a 1))

; (product * 1 inc 10) equals to the (factorial 10)
; (* 4 (product (lambda (n) (/ (+ (* 4 n n) (* 4 n)) (+ (* 4 n n) (* 4 n) 1))) 1.0 inc 100)) equals to pi

; Exercise 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate2 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))

  (iter a null-value))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Exercise 1.35
(define golden-section (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; Exercise 1.36
(define (fixed-point-trace f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  

  (display first-guess)
  (newline)
  (try first-guess))

; >  (fixed-point-trace (lambda (x) (/ (log 1000) (log x))) 10.0)
; 10.0
; 2.9999999999999996
; 6.2877098228681545
; 3.7570797902002955
; 5.218748919675316
; 4.1807977460633134
; 4.828902657081293
; 4.386936895811029
; 4.671722808746095
; 4.481109436117821
; 4.605567315585735
; 4.522955348093164
; 4.577201597629606
; 4.541325786357399
; 4.564940905198754
; 4.549347961475409
; 4.5596228442307565
; 4.552843114094703
; 4.55731263660315
; 4.554364381825887
; 4.556308401465587
; 4.555026226620339
; 4.55587174038325
; 4.555314115211184
; 4.555681847896976
; 4.555439330395129
; 4.555599264136406
; 4.555493789937456
; 4.555563347820309
; 4.555517475527901
; 4.555547727376273
; 4.555527776815261
; 4.555540933824255
; 4.555532257016376

; > (fixed-point-trace (lambda (x) (average x (/ (log 1000) (log x)))) 10.0)
; 10.0
; 6.5
; 5.095215099176933
; 4.668760681281611
; 4.57585730576714
; 4.559030116711325
; 4.55613168520593
; 4.555637206157649
; 4.55555298754564
; 4.555538647701617
; 4.555536206185039

; Exercise 1.37
(define (cont-fact n d k)
  (define (iter i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i) (+ (d i) (iter (+ i 1))))))

  (iter 1))

(define (cont-fact2 n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))

  (iter (- k 1) (/ (n k) (d k))))

; > (cont-fact (lambda (x) 1.0) (lambda (x) 1.0) 10)
; 0.6179775280898876
; > (cont-fact2 (lambda (x) 1.0) (lambda (x) 1.0) 11)
; 0.6180555555555556

(define (euler-d n)
  (if (= (remainder (+ n 1) 3) 0)
      (* 2 (/ (+ n 1) 3))
      1))

; > (cont-fact (lambda (x) 1.0) euler-d 10)
; 0.7182817182817183
; > (cont-fact (lambda (x) 1.0) euler-d 20)
; 0.7182818284590452

; Exercise 1.39
(define (tan-cf x k)
  (define (iter acc-x acc-i i)
    (if (= i k)
        (/ acc-x acc-i)
        (/ acc-x (- acc-i (iter (* acc-x x) (+ acc-i 2) (+ i 1))))))

  (iter x 1 1))

; > (tan 1.0)
; 1.5574077246549023
; > (tan-cf 1.0 9)
; 1.557407724654902

(define (sqrt2 x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (sqrt3 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x 0.00001)) (g x)) 0.00001)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt4 x)
  (newton-method (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt5 x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (cube-root2 x)
  (fixed-point-of-transform (lambda (y) (/ x (square y))) average-damp 1.0))

; Exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

; > (((double (double double)) inc) 5)
; 21

; Exercise 1.42
(define (compose2 f g)
  (lambda (x) (f (g x))))

; > ((compose2 square inc) 6)
; 49

; Exercise 1.43
(define (repeated f n)
  (define (iter i result)
    (if (= i n)
        result
        (iter (inc i) (compose2 f result))))
  
  (iter 1 f))

; > ((repeated square 2) 5)
; 625

; Exercise 1.44
(define (smooth f)
  (define dx 0.0001)
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

; > (sin 0.5)
; 0.479425538604203
; > ((smooth sin) 0.5)
; 0.47942553700611795
; > (((repeated smooth 1) sin) 0.5)
; 0.47942553700611795
; > (((repeated smooth 2) sin) 0.5)
; 0.4794255354080328
; > (((repeated smooth 3) sin) 0.5)
; 0.47942553380994773

; Exercise 1.45
; > (fixed-point-of-transform (lambda (y) (/ 4.0 y)) average-damp 1.0)
; 2.000000000000002
; > (fixed-point-of-transform (lambda (y) (/ 9.0 y)) average-damp 1.0)
; 3.0
; > (fixed-point-of-transform (lambda (y) (/ 8.0 (* y y))) average-damp 1.0)
; 1.9999981824788517
; > (fixed-point-of-transform (lambda (y) (/ 27.0 (* y y))) average-damp 1.0)
; 2.9999972321057697

; > (fixed-point-of-transform (lambda (y) (/ 16.0 (* y y y))) average-damp 1.0)
; . . user break
; > (fixed-point-of-transform (lambda (y) (/ 16.0 (* y y y))) (repeated average-damp 2) 1.0)
; 2.0000000000021965
; > (fixed-point-of-transform (lambda (y) (/ 81.0 (* y y y))) (repeated average-damp 2) 1.0)
; 3.000000000000033

; > (fixed-point-of-transform (lambda (y) (/ 32.0 (* y y y y))) (repeated average-damp 2) 1.0)
; 2.0000015129957607
; > (fixed-point-of-transform (lambda (y) (/ 64.0 (* y y y y y))) (repeated average-damp 2) 1.0)
; 2.0000029334662086
; > (fixed-point-of-transform (lambda (y) (/ 128.0 (* y y y y y y))) (repeated average-damp 2) 1.0)
; 2.0000035538623377
; > (fixed-point-of-transform (lambda (y) (/ 256.0 (* y y y y y y y))) (repeated average-damp 2) 1.0)
; . . user break
; > (fixed-point-of-transform (lambda (y) (/ 256.0 (* y y y y y y y))) (repeated average-damp 3) 1.0)
; 2.0000000000039666

(define (n-root x n)
  (define average-count (floor (log n 2)))

  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1)))) (repeated average-damp average-count) 1.0))

; > (n-root 4 2)
; 2.000000000000002
; > (n-root 8 3)
; 1.9999981824788517
; > (n-root 16 4)
; 2.0000000000021965
; > (n-root 32 5)
; 2.000001512995761
; > (n-root 64 6)
; 2.0000029334662086
; > (n-root 128 7)
; 2.0000035538623377
; > (n-root 256 8)
; 2.000000000003967
; > (n-root 512 9)
; 1.9999997106840102
; > (n-root 1024 10)
; 2.0000011830103324

; Exercise 1.46
(define (iterative-improve good-enough? improve first-guess)
  (define (try guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
          next
          (try next))))

  (try first-guess))
        
(define (sqrt6 x)
  (define (good-enough? v1 v2) (< (abs (- v1 v2)) 0.00001))
  (define (improve y) (average y (/ x y)))

  (iterative-improve good-enough? improve 1.0))

; > (sqrt6 2.0)
; 1.4142135623746899
; > (sqrt6 256)
; 16.00000000000039
; > (sqrt6 10)
; 3.162277660168379

(define (fixed-point2 f first-guess)
  (define (good-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))

  (iterative-improve good-enough? f first-guess))

; > (fixed-point2 (lambda (x) (+ 1 (/ 1 x))) 1.0)
; 1.6180327868852458
