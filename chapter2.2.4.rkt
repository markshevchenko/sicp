#lang racket
; http://community.schemewiki.org/?sicp-ex-2.44
(provide (all-defined-out))
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1))) 

; https://stackoverflow.com/a/41507786/1051621
(define wave
  (segments->painter
   (list
    (make-segment (make-vect 0.20 0.00) (make-vect 0.35 0.50))
    (make-segment (make-vect 0.35 0.50) (make-vect 0.30 0.60))
    (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.45))
    (make-segment (make-vect 0.15 0.45) (make-vect 0.00 0.60))
    (make-segment (make-vect 0.00 0.80) (make-vect 0.15 0.65))
    (make-segment (make-vect 0.15 0.65) (make-vect 0.30 0.70))
    (make-segment (make-vect 0.30 0.70) (make-vect 0.40 0.70))
    (make-segment (make-vect 0.40 0.70) (make-vect 0.35 0.85))
    (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
    (make-segment (make-vect 0.60 1.00) (make-vect 0.65 0.85))
    (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.70))
    (make-segment (make-vect 0.60 0.70) (make-vect 0.75 0.70))
    (make-segment (make-vect 0.75 0.70) (make-vect 1.00 0.40))
    (make-segment (make-vect 1.00 0.20) (make-vect 0.60 0.48))
    (make-segment (make-vect 0.60 0.48) (make-vect 0.80 0.00))
    (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.30))
    (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00)))))

; Exercise 2.44

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; (paint (corner-split einstein 4))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
   (combine4 (corner-split painter n))))

; Exercise 2.45

(define (split first second)
  (define (rec-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (rec-split painter (- n 1))))
          (first painter (second smaller smaller)))))
  
  rec-split)

(define right-split2 (split beside below))
(define up-split2 (split below beside))

; Exercise 2.46

(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
; > (add-vect (make-vect 1 2) (make-vect 3 4))
; '(4 . 6)

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
; > (sub-vect (make-vect 3 4) (make-vect 1 2))
; '(2 . 2)

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))
; > (scale-vect 10 (make-vect 1 2))
; '(10 . 20)

; Exercise 2.47
(define (make-frame2 origin edge1 edge2)
  (list origin edge1 edge2))
; > (make-frame2 1 2 3)
; '(1 2 3)

(define (origin-frame2 frame) (car frame))
; > (origin-frame2 (make-frame2 1 2 3))
; 1

(define (edge1-frame2 frame) (cadr frame))
; > (edge1-frame2 (make-frame2 1 2 3))
; 2

(define (edge2-frame2 frame) (caddr frame))
; > (edge2-frame2 (make-frame2 1 2 3))
; 3

(define (make-frame3 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
; > (make-frame3 4 5 6)
; '(4 5 . 6)

(define (origin-frame3 frame) (car frame))
; > (origin-frame3 (make-frame3 4 5 6))
; 4

(define (edge1-frame3 frame) (car (cdr frame)))
; > (edge1-frame3 (make-frame3 4 5 6))
; 5

(define (edge2-frame3 frame) (cdr (cdr frame)))
; > (edge2-frame3 (make-frame3 4 5 6))
; 6

; Exercise 2.48
; (define (make-segment2 v1 v2)
;   (cons v1 v2))

(define (start-segment s)
  (car s))
; > (start-segment (make-segment (make-vect 1 2) (make-vect 3 4)))
; '(1 . 2)

(define (end-segment s)
  (cdr s))
; > (end-segment (make-segment (make-vect 1 2) (make-vect 3 4)))
; '(3 . 4)
