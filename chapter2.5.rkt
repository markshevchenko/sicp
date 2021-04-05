#lang sicp

; https://stackoverflow.com/a/40283917/1051621

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (apply-generic.1 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No methods for types -- APPLY-GENERIC" (list op type-tags))))))

; Chapter 2.5.1

(define (add x y) (apply-generic 'add x y))
; (define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'negate '(scheme-number) (lambda (x) (tag (- x))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'raise '(scheme-number) (lambda (x) (make-rational x 1)))

  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero-rat? x)
    (= (numer x) 0))


  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'negate '(rational) (lambda (x) (tag (make-rat (- (numer x)) (denom x)))))
  (put 'equ? '(rational rational) equ-rat?)
  (put '=zero? '(rational) =zero-rat?)
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational) (lambda (x) (make-complex-from-real-imag (/ (numer x) (denom x)) 0)))

  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

(define (install-rectangular-package)
  (define (square x) (* x x))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))


  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (x y) (tag (make-from-mag-ang x y))))

  'done)

(define (install-polar-package)
  (define (square x) (* x x))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (x y) (tag (make-from-mag-ang x y))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (=zero-complex? z)
    (and (= (real-part z) 0)
         (= (imag-part z) 0)))


  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'negate '(complex) (lambda (z) (tag (make-from-real-imag (- (real-part z)) (- (imag-part z))))))
  (put 'equ? '(complex complex) equ-complex?)
  (put '=zero? '(complex) =zero-complex?)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (x y) (tag (make-from-mag-ang x y))))

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang x y)
  ((get 'make-from-mag-ang 'complex) x y))

(install-complex-package)

; Exercise 2.77

; > (install-complex-package)
; done
; > (magnitude (make-complex-from-real-imag 3 4))
; 5

; Exercise 2.78

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Invalid tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Invalid tagged datum -- CONTENTS" datum))))

; Exercise 2.79

(define (equ? x y) (apply-generic 'equ? x y))

; > (install-rational-package)
; done
; > (equ? (make-rational 2 4) (make-rational 5 10))
; #t
; > (equ? (make-rational 1 2) (make-rational 2 1))
; #f

; Exercise 2.80

(define (=zero? x) (apply-generic '=zero? x))

; > (=zero? (make-rational 0 1))
; #t
; > (=zero? (make-rational 1 2))
; #f

; Chapter 2.5.2

(define (put-coercion source-type target-type proc)
  (put 'coercion (list source-type target-type) proc))

(define (get-coercion source-type target-type)
  (get 'coercion (list source-type target-type)))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (scheme-number->rational n)
  (make-rational (contents n) 1))

(put-coercion 'scheme-number 'rational scheme-number->rational)

(define (apply-generic.2 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                        (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                        (else (error "Method not found" (list op type-tags))))))
              (error "Method not found" (list op type-tags)))))))

; > (install-complex-package)
; done
; > (add (make-complex-from-real-imag 2 3) 8)
; (complex rectangular 10 . 3)

; Exercise 2.81a

; > (define (scheme-number->scheme-number n) n)
; > (define (complex->complex z) z)
; > (put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
; ok
; > (put-coercion 'complex 'complex complex->complex)
; ok
; > (add 2 4) --> eternal loop

; Exercise 2.81c

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error ("Method not found" (list op type-tags)))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                            (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                            (else (error "Method not found" (list op type-tags)))))))
              (error "Method not found" (list op type-tags)))))))

; Exercise 2.82

(define (find-proc-by-args op args)
    (let ((type-tags (map type-tag args)))
      (get op type-tags)))

(define (find-proc-casting-each-type op args)
  (define (iter iter-args)
    (if (null? iter-args)
        false
        (let ((casted-args (cast-to-type (type-tag (car iter-args)) args)))
          (let ((proc (find-proc-by-args op casted-args)))
            (if proc
                (cons proc casted-args)
                (iter (cdr iter-args)))))))

  (iter args))

(define (cast-arg-to-type new-type-tag arg)
  (cond ((eq? new-type-tag (type-tag arg)) arg)
        (else (let ((cast-proc (get-coercion (type-tag arg) new-type-tag)))
                (if cast-proc
                    (apply cast-proc (list (contents arg)))
                    '(unknown ()))))))

(define (cast-to-type new-type-tag args)
  (if (null? args)
      '()
      (cons (cast-arg-to-type new-type-tag (car args)) (cast-to-type new-type-tag (cdr args)))))

(define (apply-generic.3 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((proc-and-casted-args (find-proc-casting-each-type op args)))
            (if (pair? proc-and-casted-args)
                (apply (car proc-and-casted-args) (map contents (cdr proc-and-casted-args)))
                (error "Method not found" (list op type-tags))))))))

; Exercise 2.83

(define (raise x) (apply-generic 'raise x))

; > (raise 2)
; (rational 2 . 1)
; > (raise (make-rational 2 1))
; (complex rectangular 2 . 0)

; Chapter 2.5.3

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2)) (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else (adjoin-term (make-term (order t1) (add (coeff t1) (coeff t2)))
                                     (add-terms (rest-terms L1) (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (negate-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t (first-term L)))
          (adjoin-term (make-term (order t) (negate (coeff t))) (rest-terms L)))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polynomes from different variables -- ADD-POLY" (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polynomes from different variables -- MUL-POLY" (list p1 p2))))

  (define (negate-poly p)
    (make-poly (variable p)
               (negate-terms (term-list p))))

  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'negate '(polynomial) (lambda (p) (tag (negate-poly p))))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  ; Exercise 2.87
  (put '=zero? '(polynomial) empty-termlist?)

  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-polynomial-package)

; Exercise 2.88
(define (negate x) (apply-generic 'negate x))
(define (sub x y) (add x (negate y)))

; Exercise 2.89

(define (make-term.2 order coeff) (list order coeff))
(define (order.2 term) (car term))
(define (coeff.2 term) (cadr term))

(define (the-empty-termlist.2) '())
(define (empty-termlist.2? term-list) (null? term-list))

(define (rest-terms.2 term-list) (cdr term-list))

(define (first-term.2 term-list)
  (let ((first-term-order (- (length term-list) 1))
        (first-term-coeff (car term-list)))
    (make-term.2 first-term-order first-term-coeff)))

(define (append-zeros-to-list list n)
  (if (= n 0)
      list
      (append-zeros-to-list (cons 0 list) (dec n))))

(define (adjoin-term.2 term term-list)
  (let ((new-order (order.2 term))
        (new-coeff (coeff.2 term))
        (current-order (- (length term-list) 1)))
    (cons new-coeff (append-zeros-to-list term-list (- new-order current-order 1)))))

; > (first-term.2 '(1 2 3))
; (2 1)
; > (adjoin-term.2 (make-term.2 3 8) '(1 2))
; (8 0 1 2)