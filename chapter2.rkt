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

(define (cons2 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument is nor 0 or 1 -- CONS " m))))

  dispatch)

(define (car2 z) (z 0))
(define (cdr2 z) (z 1))

; Exercise 2.4
(define (cons3 x y)
  (lambda (m) (m x y)))

(define (car3 z)
  (z (lambda (p q) p)))

(define (cdr3 z)
  (z (lambda (p q) q)))

; (define p2 (cons3 2 3))
; (car3 p2)
; > 2
; (cdr3 p2)
; > 3

; Exercise 2.5
(define (cons4 x y)
  (define (pow a n)
    (cond ((= n 0) 1)
          ((= n 1) a)
          (else (* a (pow a (dec n))))))

  (* (pow 2 x) (pow 3 y)))

(define (car4 z)
  (define (count-div2 n result)
    (if (not (= (remainder n 2) 0))
        result
        (count-div2 (/ n 2) (inc result))))

  (count-div2 z 0))

(define (cdr4 z)
  (define (count-div3 n result)
    (if (not (= (remainder n 3) 0))
        result
        (count-div3 (/ n 3) (inc result))))

  (count-div3 z 0))

; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))

(define (make-interval lower-bound upper-bound)
  (cons lower-bound upper-bound))

; Exercise 2.7
(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

; Exercise 2.10
(define (div-interval x y)
  (if (and (< (lower-bound y) 0)
           (> (upper-bound y) 0))
      (error "Divisor interval contains 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

; Exercise 2.11
(define (mul-interval2 x y)
  (let ((xmin (lower-bound x))
        (xmax (upper-bound x))
        (ymin (lower-bound y))
        (ymax (upper-bound y)))
    (cond ((and (< xmax 0) (< ymax 0)) (make-interval (* xmax ymax) (* xmin ymin)))
          ((and (< xmax 0) (> ymin 0)) (make-interval (* xmin ymax) (* xmax ymin)))
          ((and (> xmin 0) (< ymax 0)) (make-interval (* xmax ymin) (* xmin ymax)))
          ((and (> xmin 0) (> ymin 0)) (make-interval (* xmin ymin) (* xmax ymax)))
          ((and (< xmin 0) (> xmax 0) (< ymax 0)) (make-interval (* xmax ymin) (* xmin ymin)))
          ((and (< xmin 0) (> xmax 0) (> ymin 0)) (make-interval (* xmin ymax) (* xmax ymax)))
          ((and (< xmax 0) (< ymin 0) (> ymax 0)) (make-interval (* xmin ymax) (* xmin ymin)))
          ((and (> xmin 0) (< ymin 0) (> ymax 0)) (make-interval (* xmax ymin) (* xmax ymax)))
          (else (make-interval (min (* xmin ymax) (* xmax ymin)) (max (* xmin ymin) (* xmax ymax)))))))

(define (make-center-width center width)
  (make-interval (- center width) (+ center width)))
(define (center interval)
  (/ (+ (lower-bound interval) (upper-bound interval)) 2))
(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

; Exercise 2.12
(define (make-center-percent center percent)
  (make-center-width center (* (abs center) percent)))

(define (percent interval)
  (/ (width interval) (abs (center interval))))

(define (list-ref2 items n)
  (if (= n 0)
      (car items)
      (list-ref2 (cdr items) (dec n))))

(define (length2 items)
  (if (null? items)
      0
      (inc (length2 (cdr items)))))

; Exercise 2.17
(define (last-pair items)
  (define (iter tail head)
    (if (null? tail)
        (list head)
        (iter (cdr tail) (car tail))))

  (iter items nil))

; > (last-pair (list 23 72 149 34))
; (34)

; Exersice 2.18
(define (reverse2 items)
  (define (iter tail acc)
    (if (null? tail)
        acc
        (iter (cdr tail) (cons (car tail) acc))))

  (iter items '()))

; > (reverse2 (list 1 4 9 16 25))
; (25 16 9 4 1)

; Exercise 2.19
(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; > (cc 100 us-coins)
; 292
; > (cc 100 uk-coins)
; 104561

; Exercise 2.20
(define (same-parity head . tail)
  (define (filter p items)
    (cond ((null? items) '())
          ((p (car items)) (cons (car items) (filter p (cdr items))))
          (else (filter p (cdr items)))))

  (cons head (if (even? head)
                 (filter even? tail)
                 (filter odd? tail))))

; > (same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)
; > (same-parity 2 3 4 5 6 7)
; (2 4 6)

; Exercise 2.21
(define (square-list1 items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list1 (cdr items)))))
; > (square-list1 (list 1 2 3 4 5))
; (1 4 9 16 25)

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))
; > (square-list2 (list 1 2 3 4 5))
; (1 4 9 16 25)

; Exercise 2.23
(define (for-each2 f items)
  (map f items)
  #t)
; > (for-each2 (lambda (x) (newline) (display x)) (list 1 2 3 4))
;
; 1
; 2
; 3
; 4#t

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

; Exercise 2.25
; > (car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
; 7
; > (car (car '((7))))
; 7
; > (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))
; 7

; Exercise 2.26
; > (append '(1 2 3) '(4 5 6))
; (1 2 3 4 5 6)
; > (cons '(1 2 3) '(4 5 6))
; ((1 2 3) 4 5 6)
; > (list '(1 2 3) '(4 5 6))
; ((1 2 3) (4 5 6))

; Exercise 2.27
(define (deep-reverse x)
  (reverse (map (lambda (y) (if (pair? y) (deep-reverse y) y)) x)))

; > (deep-reverse '((1 2) (3 4)))
; ((4 3) (2 1))

; Exercise 2.28
(define (fringe tree)
  (cond ((null? tree) nil)
        ((pair? (car tree)) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (cons (car tree) (fringe (cdr tree))))))

; > (fringe '(1 2 (3 4 (5 6) 7 8) 9 10))
; (1 2 3 4 5 6 7 8 9 10)

; Exercise 2.29
(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))
(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)

(define (total-weight mobile)
  (define (weight branch)
    (if (pair? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))

  (+ (weight (left-branch mobile)) (weight (right-branch mobile))))

; > (total-weight (make-mobile (make-branch 10 20) (make-branch 20 (make-mobile (make-branch 30 4) (make-branch 10 5)))))
; 29

(define (balanced?-weight mobile)
  (let ((left-length (branch-length (left-branch mobile)))
        (right-length (branch-length (right-branch mobile)))
        (left-structure (branch-structure (left-branch mobile)))
        (right-structure (branch-structure (right-branch mobile))))

    (let ((left-balanced?-weight (if (pair? left-structure)
                                     (balanced?-weight left-structure)
                                     (cons #t left-structure)))
          (right-balanced?-weight (if (pair? right-structure)
                                      (balanced?-weight right-structure)
                                      (cons #t right-structure))))

      (let ((left-balanced? (car left-balanced?-weight))
            (left-weight (cdr left-balanced?-weight))
            (right-balanced? (car right-balanced?-weight))
            (right-weight (cdr right-balanced?-weight)))

        (cons (and left-balanced?
                   right-balanced?
                   (= (* left-length left-weight) (* right-length right-weight)))
              (+ left-weight right-weight))))))

(define (balanced? mobile)
  (car (balanced?-weight mobile)))

; > (define m1 (make-mobile (make-branch 10 2) (make-branch 5 4)))
; > (define m2 (make-mobile (make-branch 20 6) (make-branch 15 8)))
; > (define m3 (make-mobile (make-branch 7 m1) (make-branch 3 m2)))
; > (balanced? m3)
; #t

; Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
; > (square-tree (list 1
;                      (list 2 (list 3 4) 5)
;                      (list 6 7)))
; (1 (4 (9 16) 25) (36 49))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (* sub-tree sub-tree)))
       tree))
; > (square-tree2 (list 1
;                       (list 2 (list 3 4) 5)
;                       (list 6 7)))
; (1 (4 (9 16) 25) (36 49))

; Exercise 2.31
(define (square x) (* x x))
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))
(define (square-tree3 tree) (tree-map square tree))
; > (square-tree3 (list 1
;                       (list 2 (list 3 4) 5)
;                       (list 6 7)))
; (1 (4 (9 16) 25) (36 49))

; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (ss) (cons (car s) ss)) rest)))))
; > (subsets (list 1 2 3))
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; Exercise 2.33

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; > (map square (list 1 2 3 4 5))
; (1 4 9 16 25)
(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
; > (map2 square (list 1 2 3 4 5))
; (1 4 9 16 25)

; > (append (list 1 2 3) (list 4 5 6))
; (1 2 3 4 5 6)
(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))
; > (append2 (list 1 2 3) (list 4 5 6))
; (1 2 3 4 5 6)

; > (length (list 1 2 3 4))
; 4
(define (length3 sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
; > (length3 (list 1 2 3 4))
; 4

; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

; > (horner-eval 2 (list 1 3 0 5 0 1))
; 79

; Exercise 2.35
(define (count-leaves2 t)
  (accumulate + 0 (map (lambda (x)
                         (cond ((null? x) 0)
                               ((not (pair? x)) 1)
                               (else (count-leaves2 x))))
                       t)))
; > (count-leaves '(((1 2) 3 4) ((1 2) 3 4)))
; 8
; > (count-leaves2 '(((1 2) 3 4) ((1 2) 3 4)))
; 8

; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; >  (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
; (22 26 30)
