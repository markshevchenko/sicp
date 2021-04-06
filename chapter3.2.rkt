#lang sicp

; Chapter 3.3.1

; > (define x '((a b) c d))
; > (define y '(e f))
; > (set-car! x y)
; > x
; ((e f) c d)
; > y
; (e f)

; > (define x '((a b) c d))
; > (define y '(e f))
; > (set-cdr! x y)
; > x
; ((a b) e f)
; > y
; (e f)

; Exercise 3.12

; (define (append x y)
;   (if (null? x)
;       y
;       (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; > (define x '(a b))
; > (define y '(c d))
; > (define z (append x y))
; > 
; > z
; (a b c d)
; > (cdr x)
; (b)
; > (define w (append! x y))
; > w
; (a b c d)
; > (cdr x)
; (b c d)

; Exercise 3.13

; > (define (make-cycle x)
; >  (set-cdr! (last-pair x) x)
; >  x)
; > 
; > (define z (make-cycle '(a b c)))
; > z
; #0=(a b c . #0#)
; > (last-pair z)
; user break

; Exercise 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))

  (loop x '()))

; > (define v '(a b c d))
; > v
; (a b c d)
; > (define w (mystery v))
; > v
; (a)
; > w
; (d c b a)

; Exercise 3.16

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; > (define x (cons 1 2))
; > (define y (cons 3 4))
; > (define z3 (cons x y))
; > (count-pairs z3)
; 3
; > (define w (cons 5 x))
; > (define z4 (cons x w))
; > (count-pairs z4)
; 4
; > (define v (cons x x))
; > (define z7 (cons v v))
; > (count-pairs z7)
; 7
; > (set-cdr! x x)
; > (count-pairs x)
; Interactions disabled; out of memory

; Exercise 3.17

(define (contains? pair pairs)
  (cond ((null? pairs) false)
        ((eq? pair (car pairs)) true)
        (else (contains? pair (cdr pairs)))))

(define (count-pairs.2 x)
  (define visited '())
  (define (iter item)
    (cond ((contains? item visited) 0)
          ((not (pair? item)) 0)
          (else (begin (set! visited (cons item visited))
                       (+ (iter (car item))
                          (iter (cdr item))
                          1)))))
  (iter x))

; > (define x (cons 1 2))
; > (define y (cons 3 4))
; > (define z3 (cons x y))
; > (count-pairs.2 z3)
; 3
; > (define w (cons 5 x))
; > (define z4 (cons w x))
; > (count-pairs.2 z4)
; 3
; > (define v (cons x x))
; > (define z7 (cons v v))
; > (count-pairs.2 z7)
; 3

; Exercise 3.18

(define (cycle? pair)
  (define visited '())
  (define (iter item)
    (cond ((contains? item visited) true)
          ((not (pair? item)) false)
          (else (begin (set! visited (cons item visited))
                       (or (iter (car item)) (iter (cdr item)))))))
  (iter pair))

; > (define x '(1 2))
; > (cycle? x)
; #f
; > (set-cdr! x x)
; > (cycle? x)
; #t