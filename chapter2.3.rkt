#lang sicp

; Chapter 2.3.1

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

; Chapter 2.3.2

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

; Chapter 2.3.3

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

; Exercise 2.59

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

; Exercise 2.61

(define (element-of-set3? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set3? x (cdr set)))))
; > (element-of-set3? 4 '(1 2 3 5 6 7))
; #f

(define (intersection-set3 set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set3 (cdr set1)
                                        (cdr set2))))
              ((< x1 x2)
               (intersection-set3 (cdr set1) set2))
              ((< x2 x1)
               (intersection-set3 set1 (cdr set2)))))))
; > (intersection-set3 '(1 2 3 4) '(3 4 5 6))
; (3 4)

(define (adjoin-set3 x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set3 x (cdr set))))))
; > (adjoin-set3 3 '())
; (3)
; > (adjoin-set3 3 '(1 2 3 4 5))
; (1 2 3 4 5)
; > (adjoin-set3 3 '(1 2 4 5))
; (1 2 3 4 5)
; > (adjoin-set3 3 '(5 6 7 8))
; (3 5 6 7 8)

; Exercise 2.62

(define (union-set3 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set3 (cdr set1) (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set3 (cdr set1) set2)))
                      ((< x2 x1)
                       (cons x2 (union-set3 set1 (cdr set2)))))))))
; > (union-set3 '(1 2 3) '())
; (1 2 3)
; > (union-set3 '() '(1 2 3))
; (1 2 3)
; > (union-set3 '(1 2 3) '(1 2 3))
; (1 2 3)
; > (union-set3 '(1 2 3 4) '(3 4 5 6))
; (1 2 3 4 5 6)
; > (union-set3 '(1 2) '(3 4))
; (1 2 3 4)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
; (make-tree 3 (make-tree 1 nil nil) (make-tree 5 nil nil))
; (3 (1 () ()) (5 () ()))
(define (element-of-set4? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set4? x (left-branch set)))
        ((> x (entry set))
         (element-of-set4? x (right-branch set)))))
; > (element-of-set4? 5 '(3 (1 () ()) (5 () ())))
; #t
; > (element-of-set4? 6 '(3 (1 () ()) (5 () ())))
; #f

(define (adjoin-set4 x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set4 x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set4 x (right-branch set))))))
; > (adjoin-set4 3 (adjoin-set4 1 (adjoin-set4 5 '())))
; (5 (1 () (3 () ())) ())
; > (adjoin-set4 5 (adjoin-set4 1 (adjoin-set4 3 '())))
; (3 (1 () ()) (5 () ()))

; Exercise 2.63a

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
   (copy-to-list tree '()))

; > (tree->list-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
; (1 3 5 7 9 11)
; > (tree->list-2 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
; (1 3 5 7 9 11)
; > (tree->list-1 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
; (1 3 5 7 9 11)
; > (tree->list-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
; (1 3 5 7 9 11)
; > (tree->list-1 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
; (1 3 5 7 9 11)
; > (tree->list-2 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
; (1 3 5 7 9 11)

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
; > (list->tree '(1 2 3 5 7 9 11))
; (5 (2 (1 () ()) (3 () ())) (9 (7 () ()) (11 () ())))

; Exercise 2.62

(define (union-set4 set1 set2)
  (list->tree (union-set3 (tree->list-2 set1)
                          (tree->list-2 set2))))
; > (union-set4 '(2 (1 () ()) (3 () ())) '(4 (3 () ()) (5 () ())))
; (3 (1 () (2 () ())) (4 () (5 () ())))

(define (intersection-set4 set1 set2)
  (list->tree (intersection-set3 (tree->list-2 set1)
                                   (tree->list-2 set2))))
; > (intersection-set4 '(2 (1 () ()) (3 () ())) '(4 (3 () ()) (5 () ())))
; (3 () ())

; Exercise 2.66

(define (make-record key payload)
  (cons key payload))
; > (make-record 3 '("foo"))
; (3 "foo")

(define (key record)
  (car record))
(define (payload record)
  (cdr record))
; > (key (make-record 3 '("foo")))
; 3
; > (payload (make-record 3 '("foo")))
; ("foo")

(define (make-node record left right)
  (list record left right))
; > (make-node (make-record 3 '("foo"))
;              (make-node (make-record 1 '("bar")) '() '())
;              (make-node (make-record 5 '("baz")) '() '()))
; ((3 "foo") ((1 "bar") () ()) ((5 "baz") () ()))

(define (left node)
  (cadr node))
; > (left '((3 "foo") ((1 "bar") () ()) ((5 "baz") () ())))
; ((1 "bar") () ())

(define (right node)
  (caddr node))
; > (right '((3 "foo") ((1 "bar") () ()) ((5 "baz") () ())))
; ((5 "baz") () ())

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (car set-of-records))) true)
        ((< given-key (key (car set-of-records))) (lookup given-key (left set-of-records)))
        ((> given-key (key (car set-of-records))) (lookup given-key (right set-of-records)))))
; > (lookup 1 '((3 "foo") ((1 "bar") () ()) ((5 "baz") () ())))
; #t
; > (lookup 3 '((3 "foo") ((1 "bar") () ()) ((5 "baz") () ())))
; #t
; > (lookup 5 '((3 "foo") ((1 "bar") () ()) ((5 "baz") () ())))
; #t
; > (lookup 0 '((3 "foo") ((1 "bar") () ()) ((5 "baz") () ())))
; #f
; > (lookup 2 '((3 "foo") ((1 "bar") () ()) ((5 "baz") () ())))
; #f
; > (lookup 6 '((3 "foo") ((1 "bar") () ()) ((5 "baz") () ())))
; #f

; Chapter 2.3.4

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-br tree) (car tree))
(define (right-br tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-br branch))
        ((= bit 1) (right-br branch))
        (else (error "Bad bit"))))

(define (adjoin-hset x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-hset x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-hset (make-leaf (car pair)
                                (cadr pair))
                     (make-leaf-set (cdr pairs))))))

; > (make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
; ((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))

; Exercise 2.67

; (define sample-tree
;   (make-code-tree (make-leaf 'A 4)
;                   (make-code-tree
;                    (make-leaf 'B 2)
;                    (make-code-tree (make-leaf 'D 1)
;                                    (make-leaf 'C 1)))))
; > sample-tree
; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

; (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; (decode sample-message sample-tree)
; (A D A B B C A)

; Exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left-br tree))) (cons 0 (encode-symbol symbol (left-br tree))))
        ((element-of-set? symbol (symbols (right-br tree))) (cons 1 (encode-symbol symbol (right-br tree))))))

; > (encode-symbol 'A '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8))
; (0)
; > (encode-symbol 'B '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8))
; (1 0)
; > (encode-symbol 'C '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8))
; (1 1 1)
; > (encode-symbol 'D '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8))
; (1 1 0)
; > (encode '(A D A B B C A) '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8))
; (0 1 1 0 0 1 0 1 0 1 1 1 0)

; Exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (adjoin-leaf-set x set)
  (cond ((null? set) (list x))
        ((or (= (weight x) (weight (car set)))
             (< (weight x) (weight (car set))))
         (cons x set))
        (else (cons (car set) (adjoin-leaf-set x (cdr set))))))

; > (adjoin-leaf-set '((leaf D 1) (leaf C 1) (D C) 2) '((leaf B 2) (leaf A 4)))
; (((leaf D 1) (leaf C 1) (D C) 2) (leaf B 2) (leaf A 4))
; > (adjoin-leaf-set '((leaf D 1) (leaf C 2) (D C) 3) '((leaf B 2) (leaf A 4)))
; ((leaf B 2) ((leaf D 1) (leaf C 2) (D C) 3) (leaf A 4))

(define (successive-merge leaf-set)
  (cond ((null? (cdr leaf-set)) (car leaf-set))
        (else (let ((left (cadr leaf-set))
                    (right (car leaf-set)))
                (successive-merge (adjoin-leaf-set (make-code-tree left right) (cdr (cdr leaf-set))))))))

; > (successive-merge '((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4)))
; ((leaf A 4) ((leaf B 2) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 4) (A B C D) 8)
; >  (decode '(0 1 1 0 0 1 0 1 0 1 1 1 0)
;            '((leaf A 4) ((leaf B 2) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 4) (A B C D) 8))
; (A C A B B D A)

; Exercise 2.70

; > (generate-huffman-tree '((NA 16) (YIP 9) (SHA 3) (A 2) (GET 2) (JOB 2) (BOOM 1) (WAH 1)))
; ((((((leaf A 2) (leaf GET 2) (A GET) 4) (leaf SHA 3) (A GET SHA) 7)
;    ((leaf JOB 2) ((leaf BOOM 1) (leaf WAH 1) (BOOM WAH) 2) (JOB BOOM WAH) 4)
;    (A GET SHA JOB BOOM WAH)
;    11)
;   (leaf YIP 9)
;   (A GET SHA JOB BOOM WAH YIP)
;   20)
;  (leaf NA 16)
;  (A GET SHA JOB BOOM WAH YIP NA)
;  36)

; > (encode '(GET A JOB
;             SHA NA NA NA NA NA NA NA NA
;             GET A JOB
;             SHA NA NA NA NA NA NA NA NA
;             WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
;             SHA BOOM)
;           '((((((leaf A 2) (leaf GET 2) (A GET) 4) (leaf SHA 3) (A GET SHA) 7)
;    ((leaf JOB 2) ((leaf BOOM 1) (leaf WAH 1) (BOOM WAH) 2) (JOB BOOM WAH) 4)
;    (A GET SHA JOB BOOM WAH)
;    11)
;   (leaf YIP 9)
;   (A GET SHA JOB BOOM WAH YIP)
;   20)
;  (leaf NA 16)
;  (A GET SHA JOB BOOM WAH YIP NA)
;  36))
; (0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 1 0
;  0 0 0 0 0 0 1 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 1 1 1 0 1 0 1 0 1 0
;  1 0 1 0 1 0 1 0 1 0 1 0 0 0 1 0 0 1 1 0)

; Exercise 2.71

; > (generate-huffman-tree '((E 16) (D 8) (C 4) (B 2) (A 1)))
; ((leaf E 16) ((leaf D 8) ((leaf C 4) ((leaf B 2) (leaf A 1) (B A) 3) (C B A) 7) (D C B A) 15) (E D C B A) 31)

; > (generate-huffman-tree '((J 512) (I 256) (H 128) (G 64) (F 32) (E 16) (D 8) (C 4) (B 2) (A 1)))
; ((leaf J 512)
;  ((leaf I 256)
;   ((leaf H 128)
;    ((leaf G 64)
;     ((leaf F 32)
;      ((leaf E 16) ((leaf D 8) ((leaf C 4) ((leaf B 2) (leaf A 1) (B A) 3) (C B A) 7) (D C B A) 15) (E D C B A) 31)
;      (F E D C B A)
;      63)
;     (G F E D C B A)
;     127)
;    (H G F E D C B A)
;    255)
;   (I H G F E D C B A)
;   511)
;  (J I H G F E D C B A)
;  1023)
