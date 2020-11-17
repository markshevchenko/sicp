#lang scheme
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

(define (prime? n)
    (define (divides? a b) (= (remainder b a) 0))
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (inc test-divisor)))))
    
    (= n (find-divisor n 2)))

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

(define (cubic a b c)
    (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(define (double f)
    (lambda (x) (f (f x))))

(define (compose2 f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (define (iter i result)
      (if (= i n)
          result
          (iter (inc i) (compose2 f result))))
    (iter 1 f))