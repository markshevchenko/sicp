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
; > (define x '(1 2))
; > (set-cdr! x x)
; > (count-pairs.2 x)
; 1

; Exercise 3.18

(define (cycle? pair)
  (define (iter item visited)
    (cond ((null? item) false)
          ((contains? item visited) true)
          (else (iter (cdr item) (cons item visited)))))
  (iter pair '()))

; > (define lst '(1 2 3 4 5 6))
; > (cycle? lst)
; #f
; > (set-cdr! (last-pair lst) lst)
; > (cycle? lst)
; #t

; Exercise 3.19

(define (cycle.2? pair)
  (define (iter item-fast item-slow n)
    (cond ((null? item-fast) false)
          ((eq? item-fast item-slow) true)
          (else (if (= (remainder n 2) 0)
                    (iter (cdr item-fast) item-slow (inc n))
                    (iter (cdr item-fast) (cdr item-slow) (inc n))))))

  (iter (cdr pair) pair 1))

; > (define lst '(1 2 3 4 5 6))
; > (cycle.2? lst)
; #f
; > (set-cdr! (last-pair lst) lst)
; > (cycle.2? lst)
; #t

; ----------------------------------------

(define (cons.2 x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Unknown operation -- CONS.2" m))))

  dispatch)

(define (car.2 z) (z 'car))
(define (cdr.2 z) (z 'cdr))
(define (set-car.2! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr.2! z new-value)
  ((z 'set-cdr!) new-value)
  z)

; > (define x (cons.2 1 2))
; > (car.2 x)
; 1
; > (cdr.2 x)
; 2
; > (set-car.2! x 3)
; #<procedure:...p/chapter3.2.rkt:186:2>
; > (car.2 x)
; 3
; > (set-cdr.2! x 4)
; #<procedure:...p/chapter3.2.rkt:186:2>
; > (cdr.2 x)
; 4

; Chapter 3.3.2

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "Empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

; Exercise 3.21

(define (print-queue queue)
  (define (iter next-ptr rear-ptr)
    (cond ((eq? next-ptr rear-ptr) (display (car next-ptr)))
          (else (display (car next-ptr))
                (display " ")
                (iter (cdr next-ptr) rear-ptr))))
  
  (cond ((empty-queue? queue) (display "()"))
        (else
         (display "(")
         (iter (front-ptr queue) (rear-ptr queue))
         (display ")")))
  
  (newline))

; > (define q (make-queue))
; > (print-queue q)
; ()
; > (insert-queue! q 'a)
; ((a) a)
; > (print-queue q)
; (a)
; > (insert-queue! q 'b)
; ((a b) b)
; > (print-queue q)
; (a b)
; > (delete-queue! q)
; ((b) b)
; > (print-queue q)
; (b)
; > (delete-queue! q)
; (() b)
; > (print-queue q)
; ()

; Exercise 3.22

(define (make-queue.2)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?)
             (null? front-ptr))
            ((eq? m 'front-queue)
             (if (null? front-ptr)
                 (error "Empty queue")
                 (car front-ptr)))
            ((eq? m 'insert-queue!)
             (lambda (item)
               (if (null? front-ptr)
                   (begin (set! front-ptr (cons item '()))
                          (set! rear-ptr front-ptr))
                   (begin (set-cdr! rear-ptr (cons item '()))
                          (set! rear-ptr (cdr rear-ptr))))))
            ((eq? m 'delete-queue!)
             (if (null? front-ptr)
                 (error "Empty queue")
                 (let ((top (car front-ptr)))
                   (set! front-ptr (cdr front-ptr))
                   top)))))

    dispatch))

(define (empty-queue.2? queue) (queue 'empty-queue?))
(define (front-queue.2 queue) (queue 'front-queue))
(define (insert-queue.2! queue item) ((queue 'insert-queue!) item))
(define (delete-queue.2! queue) (queue 'delete-queue!))

; > (define q (make-queue.2))
; > (empty-queue.2? q)
; #t
; > (insert-queue.2! q 5)
; > (empty-queue.2? q)
; #f
; > (front-queue.2 q)
; 5
; > (insert-queue.2! q 6)
; > (delete-queue.2! q)
; 5
; > (front-queue.2 q)
; 6
; > (delete-queue.2! q)
; 6
; > (empty-queue.2? q)
; #t

; Exercise 3.23

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (make-node value fore-ptr back-ptr) (cons value (cons fore-ptr back-ptr)))
    (define (front-insert! value)
      (if (null? front-ptr)
          (let ((node (make-node value '() '())))
            (set! front-ptr node)
            (set! rear-ptr node))
          (let ((old-front-ptr front-ptr))
            (set! front-ptr (make-node value '() front-ptr))
            (set-car! (cdr old-front-ptr) front-ptr))))
    (define (rear-insert! value)
      (if (null? rear-ptr)
          (let ((node (make-node value '() '())))
            (set! front-ptr node)
            (set! rear-ptr node))
          (let ((old-rear-ptr rear-ptr))
            (set! rear-ptr (make-node value rear-ptr '()))
            (set-cdr! (cdr old-rear-ptr) rear-ptr))))
    (define (print-deque node)
      (if (eq? node rear-ptr)
          (display (car node))
          (begin (display (car node))
                 (display " ")
                 (print-deque (cddr node)))))
          
    (define (dispatch m)
      (cond ((eq? m 'empty-deque?) (null? front-ptr))
            ((eq? m 'front-deque)
             (if (null? front-ptr)
                 (error "Empty deque")
                 (car front-ptr)))
            ((eq? m 'rear-deque)
             (if (null? front-ptr)
                 (error "Empty deque")
                 (car rear-ptr)))
            ((eq? m 'front-insert-deque!) front-insert!)
            ((eq? m 'rear-insert-deque!) rear-insert!)
            ((eq? m 'front-delete-deque!)
             (cond ((null? front-ptr)
                    (error "Empty deque"))
                   ((eq? front-ptr rear-ptr)
                    (let ((deleted-value (car front-ptr)))
                      (set! front-ptr '())
                      (set! rear-ptr '())
                      deleted-value))
                   (else
                    (let ((deleted-value (car front-ptr)))
                      (set! front-ptr (cddr front-ptr))
                      (set-car! (cdr front-ptr) '())
                      deleted-value))))
            ((eq? m 'rear-delete-deque!)
             (cond ((null? front-ptr)
                    (error "Empty deque"))
                   ((eq? front-ptr rear-ptr)
                    (let ((deleted-value (car front-ptr)))
                      (set! front-ptr '())
                      (set! rear-ptr '())
                      deleted-value))
                   (else
                    (let ((deleted-value (car rear-ptr)))
                      (set! rear-ptr (cadr rear-ptr))
                      (set-cdr! (cdr rear-ptr) '())
                      deleted-value))))
            ((eq? m 'print-deque)
             (if (null? front-ptr)
                 (display "()")
                 (begin (display "(")
                        (print-deque front-ptr)
                        (display ")")))
             (newline))
            ((eq? m 'debug-front-ptr) front-ptr)
            ((eq? m 'debug-rear-ptr) rear-ptr)))

    dispatch))

(define (empty-deque? deque) (deque 'empty-deque?))
(define (front-deque deque) (deque 'front-deque))
(define (rear-deque deque) (deque 'rear-deque))
(define (front-insert-deque! deque item) ((deque 'front-insert-deque!) item))
(define (rear-insert-deque! deque item) ((deque 'rear-insert-deque!) item))
(define (front-delete-deque! deque) (deque 'front-delete-deque!))
(define (rear-delete-deque! deque) (deque 'rear-delete-deque!))
(define (print-deque deque) (deque 'print-deque))

; > (define d (make-deque))
; > (print-deque d)
; ()
; > (front-insert-deque! d 5)
; > (print-deque d)
; (5)
; > (rear-insert-deque! d 6)
; > (print-deque d)
; (5 6)
; > (front-insert-deque! d 7)
; > (rear-insert-deque! d 8)
; > (print-deque d)
; (7 5 6 8)
; > (front-delete-deque! d)
; 7
; > (rear-delete-deque! d)
; 8
; > (front-delete-deque! d)
; 5
; > (empty-deque? d)
; #f
; > (print-deque d)
; (6)
; > (rear-delete-deque! d)
; 6
; > (empty-deque? d)
; #t

; Chapter 3.3.3

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

; > (define t (make-table))
; > (lookup 'a t)
; #f
; > (insert! 'a 2 t)
; ok
; > (lookup 'a t)
; 2

(define (lookup2 key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
    false)))

(define (insert2! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))

  'ok)

; > (define t (make-table))
; > (insert2! 'a 'b 1 t)
; ok
; > (insert2! 'a 'c 2 t)
; ok
; > (lookup2 'a 'd t)
; #f
; > (lookup2 'a 'c t)
; 2

(define (make-table2)
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
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- MAKE-TABLE.2" m))))

    dispatch))

(define operation-table (make-table2))
(define get (operation-table 'lookup))
(define put (operation-table 'insert!))
