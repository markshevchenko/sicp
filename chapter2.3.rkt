#lang sicp

; Exercise 2.53

; > (list 'a 'b 'c)
; (a b c)
; > (list (list 'george))
; ((george))
; > (cdr '((x1 x2) (y1 y2)))
; ((y1 y2))
; > (cadr '((x1 x2) (y1 y2)))
; (y1 y2)
; > (pair? (car '(a short list)))
; #f
; > (memq 'red '((red shoes) (blue socks)))
; #f
; > (memq 'red '(red shoes blue socks))
; (red shoes blue socks)

; Exercise 2.54

(define (equal2? v1 v2)
  (cond ((and (pair? v1) (pair? v2)) (and (equal2? (car v1) (car v2)) (equal2? (cdr v1) (cdr v2))))
        (else (eq? v1 v2))))

; > (equal? '(this is a list) '(this is a list))
; #t
; > (equal2? '(this is a list) '(this is a list))
; #t
; > (equal? '(this is a list) '(this is list))
; #f
; > (equal2? '(this is a list) '(this is list))
; #f
; > (equal? 'a 'a)
; #t
; > (equal2? 'a 'a)
; #t
; > (equal? 'a 'b)
; #f
; > (equal2? 'a 'b)
; #f

; Exercise 2.55

; > (car ''abracadabra)
; quote

; > (car '(quote abracadabra))
; quote

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "Unknown expression type"))))

; > (deriv '(+ x 3) 'x)
; (+ 1 0)

; > (deriv '(* x y) 'x)
; (+ (* x 0) (* 1 y))

; > (deriv '(* (* x y) (+ x 3)) 'x)
; (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum2 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product2 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum2 (deriv2 (addend exp) var)
                   (deriv2 (augend exp) var)))
        ((product? exp)
         (make-sum2
          (make-product2 (multiplier exp)
                        (deriv2 (multiplicand exp) var))
          (make-product2 (deriv2 (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "Unknown expression type"))))

; > (deriv2 '(+ x 3) 'x)
; 1

; > (deriv2 '(* x y) 'x)
; y

; > (deriv2 '(* (* x y) (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 0) 0)
        ((=number? b 1) 1)
        ((and (number? b) (number? e)) (exp (* e (log b))))
        (else (list '** b e))))

(define (deriv3 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum2 (deriv3 (addend exp) var)
                    (deriv3 (augend exp) var)))
        ((product? exp)
         (make-sum2
          (make-product2 (multiplier exp)
                         (deriv3 (multiplicand exp) var))
          (make-product2 (deriv3 (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (list '* (exponent exp) (list '** (base exp) (list '- (exponent exp) '1)) (deriv3 (base exp) var)))
        (else
         (error "Unknown expression type"))))
