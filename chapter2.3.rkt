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
(define (make-exponentiation base exponent deriv)
  (let ((power (cond ((=number? exponent 1) 1)
                     ((=number? exponent 2) base)
                     ((number? exponent) (list '** base (- exponent 1)))
                     (else (list '** base (list '- exponent 1))))))
    (cond ((or (=number? base 0) (=number? exponent 0) (=number? deriv 0)) 0)
          ((and (number? exponent) (number? deriv))
            (cond ((= (* exponent deriv) 1) power)
                  ((= exponent 1) (list '* deriv power))
                  ((= deriv 1) (list '* exponent power))
                  (else (list '* (* exponent deriv) power))))
          ((=number? deriv 1) (list '* exponent power))
          (else (list '* exponent power deriv)))))

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
         (make-exponentiation (base exp) (exponent exp) (deriv3 (base exp) var)))
        (else
         (error "Unknown expression type"))))

; > (deriv3 '(** x 3) 'x)
; (* 3 (** x 2))
; > (deriv3 '(** x 2) 'x)
; (* 2 x)
; > (deriv3 '(** x 1) 'x)
; 1
; > (deriv3 '(** x a) 'x)
; (* a (** x (- a 1)))

; Exercise 2.57

(define (augend2 s)
  (if (= (length s) 3)
      (caddr s)
      (cons '+ (cdr (cdr s)))))

(define (multiplicand2 p)
  (if (= (length p) 3)
      (caddr p)
      (cons '* (cdr (cdr p)))))

(define (deriv4 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum2 (deriv4 (addend exp) var)
                    (deriv4 (augend2 exp) var)))
        ((product? exp)
         (make-sum2
          (make-product2 (multiplier exp)
                         (deriv4 (multiplicand2 exp) var))
          (make-product2 (deriv4 (multiplier exp) var)
                         (multiplicand2 exp))))
        ((exponentiation? exp)
         (make-exponentiation (base exp) (exponent exp) (deriv4 (base exp) var)))
        (else
         (error "Unknown expression type"))))

; > (deriv4 '(* x y (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))

; Exercise 2.58a

(define (sum3? x)
    (and (pair? x) (eq? (cadr x) '+)))

(define (product3? x)
    (and (pair? x) (eq? (cadr x) '*)))

(define (exponentiation3? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (addend3 s) (car s))
(define (augend3 s) (caddr s))
(define (multiplier3 p) (car p))
(define (multiplicand3 p) (caddr p))
(define (base3 e) (car e))
(define (exponent3 e) (caddr e))

(define (make-sum3 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product3 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentiation3 base exponent deriv)
  (let ((power (cond ((=number? exponent 1) 1)
                     ((=number? exponent 2) base)
                     ((number? exponent) (list base '** (- exponent 1)))
                     (else (list base '** (list '- exponent 1))))))
    (cond ((or (=number? base 0) (=number? exponent 0) (=number? deriv 0)) 0)
          ((and (number? exponent) (number? deriv))
            (cond ((= (* exponent deriv) 1) power)
                  ((= exponent 1) (list deriv '* power))
                  ((= deriv 1) (list exponent '* power))
                  (else (list (* exponent deriv) '* power))))
          ((=number? deriv 1) (list exponent '* power))
          (else (list exponent '* (list power '* deriv))))))

(define (deriv5 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum3? exp)
         (make-sum3 (deriv5 (addend3 exp) var)
                    (deriv5 (augend3 exp) var)))
        ((product3? exp)
         (make-sum3
          (make-product3 (multiplier3 exp)
                         (deriv5 (multiplicand3 exp) var))
          (make-product3 (deriv5 (multiplier3 exp) var)
                         (multiplicand3 exp))))
        ((exponentiation3? exp)
         (make-exponentiation3 (base exp) (exponent exp) (deriv5 (base exp) var)))
        (else
         (error "Unknown expression type"))))

; > (deriv5 '(x + (3 * (x + (y + 2)))) 'x)
; 4

; Compare with previous version of the function:
; > (deriv3 '(+ x (* 3 (+ x y 2))) 'x)
; 4

; Exercise 2.59

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
; > (element-of-set? 3 '(1 2 3 4 5 6))
; #t
; > (element-of-set? 7 '(1 2 3 4 5 6))
; #f

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
; > (adjoin-set 3 '(1 2 3 4 5 6))
; (1 2 3 4 5 6)
; > (adjoin-set 7 '(1 2 3 4 5 6))
; (7 1 2 3 4 5 6)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
; > (intersection-set '(1 2 3 4) '(3 4 5 6))
; (3 4)
; > (intersection-set '(1 2 3) '(4 5 6))
; ()

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons (car set1)
               (union-set (cdr set1) set2)))))
; > (union-set '() '())
; ()
; > (union-set '(1 2) '())
; (1 2)
; > (union-set '() '(1 2))
; (1 2)
; > (union-set '(1 2) '(3 4))
; (1 2 3 4)
; > (union-set '(1 2) '(2 3))
; (1 2 3)

; Exercise 2.60

(define (element-of-set2? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; > (element-of-set2? 2 '())
; #f
; > (element-of-set2? 2 '(1 3))
; #f
; > (element-of-set2? 2 '(2 2 2))
; #t

(define adjoin-set2 cons)
(define union-set2 append)
; > (adjoin-set2 2 '(1 2 3))
; (2 1 2 3)
; > (union-set2 '(1 2 3) '(2 3 4))
; (1 2 3 2 3 4)

(define (intersection-set2 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set2? (car set1) set2)
         (cons (car set1)
               (intersection-set2 (cdr set1) set2)))
        (else (intersection-set2 (cdr set1) set2))))
; > (intersection-set2 '(1 2 3) '(2 3 4 4))
; (2 3)
; > (intersection-set2 '(1 2 2 2 3 3) '(2 3 4 4))
; (2 2 2 3 3)
