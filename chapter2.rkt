#lang sicp

(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

; (define (make-rat n d) (cons n d))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sum-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (dev-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
; > (print-rat one-half)
;
; 1/2

(define one-third (make-rat 1 3))

; > (print-rat (add-rat one-half one-third))
;
; 5/6
; > (print-rat (mul-rat one-half one-third))
;
; 1/6
; > (print-rat (add-rat one-third one-third))
;
; 6/9

; Since `make-rat` has been changed to reduce fraction:
; 2/3

; Exercise 2.1
(define (make-rat2 n d)
  (define g (gcd n d))
  (let ((rn (/ n g))
        (rd (/ d g)))
    (cond ((> rd 0) (cons rn rd))
          (else (cons (- rn) (- rd))))))

; Exercise 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment segment)
  (let ((x1 (x-point (start-segment segment)))
        (y1 (y-point (start-segment segment)))
        (x2 (x-point (end-segment segment)))
        (y2 (y-point (end-segment segment))))
    (make-point (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0))))

; > (print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 2 2))))
;
; (1.0, 1.0)

; Exercise 2.3
(define (make-rect1 left bottom right top) (cons (make-point left bottom) (make-point right top)))
(define (left-bottom-rect1 r) (car r))
(define (right-top-rect1 r) (cdr r))

(define (width-rect1 r)
  (let ((left (x-point (left-bottom-rect1 r)))
        (right (x-point (right-top-rect1 r))))
    (- right left)))

(define (height-rect1 r)
  (let ((bottom (y-point (left-bottom-rect1 r)))
        (top (y-point (right-top-rect1 r))))
    (- top bottom)))

(define (perimeter-rect r get-width get-height)
  (let ((width (get-width r))
        (height (get-height r)))
    (* 2 (+ width height))))

(define (area-rect r get-width get-height)
  (let ((width (get-width r))
        (height (get-height r)))
    (* width height)))

; > (define r1 (make-rect1 0 2 2 5))
; > (width-rect1 r1)
; 2
; > (height-rect1 r1)
; 3
; > (perimeter-rect r1 width-rect1 height-rect1)
; 10
; > (area-rect r1 width-rect1 height-rect1)
; 6

(define (make-rect2 left bottom width height) (cons (make-point left bottom) (make-point width height)))
(define left-bottom-rect2 left-bottom-rect1)
(define (width-height-rect2 r) (cdr r))
(define (width-rect2 r) (x-point (width-height-rect2 r)))
(define (height-rect2 r) (y-point (width-height-rect2 r)))

; (define r2 (make-rect2 0 2 2 3))
; > (left-bottom-rect2 r2)
; (0 . 2)
; > (perimeter-rect r2 width-rect2 height-rect2)
; 10
; > (area-rect r2 width-rect2 height-rect2)
; 6
