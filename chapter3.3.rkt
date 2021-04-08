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

; Exercise 3.24

(define (make-table.3 same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
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

; > (define t (make-table.3 =))
; > ((t 'insert!) 2 3 4)
; ok
; > ((t 'lookup) 2 3)
; 4

; Exercise 3.25

(define (make-table.4)
  (let ((local-table (list '*table*)))
    (define (lookup head-key tail-keys table)
      (if (null? tail-keys)
          (let ((record (assoc head-key (cdr table))))
            (if record
                (cdr record)
                false))
          (let ((subtable (assoc head-key (cdr table))))
            (if subtable
                (lookup (car tail-keys) (cdr tail-keys) subtable)
                false))))
             
    (define (insert! head-key tail-keys table value)
      (if (null? tail-keys)
          (let ((record (assoc head-key (cdr table))))
            (if record
                (set-cdr! record value)
                (set-cdr! table
                          (cons (cons head-key value)
                                (cdr table)))))
          (let ((subtable (assoc head-key (cdr table))))
            (if subtable
                (insert! (car tail-keys) (cdr tail-keys) subtable value)
                (let ((subtable (list head-key)))
                  (set-cdr! table (cons subtable (cdr table)))
                  (insert! (car tail-keys) (cdr tail-keys) subtable value)))))

      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup) (lambda (keys) (lookup (car keys) (cdr keys) local-table)))
            ((eq? m 'insert!) (lambda (keys value) (insert! (car keys) (cdr keys) local-table value)))
            ((eq? m 'debug) local-table)
            (else (error "Unknown operation -- MAKE-TABLE.4" m))))

    dispatch))
                                 
(define (lookup.4 table keys) ((table 'lookup) keys))
(define (insert.4! table keys value) ((table 'insert!) keys value))
(define (debug.4 table) (table 'debug))

; > (define t (make-table.4))
; > (lookup.4 t '(a b))
; #f
; > (insert.4! t '(a b) 2)
; ok
; > (insert.4! t '(a c) 3)
; ok
; > (insert.4! t '(b c) 4)
; ok
; > (lookup.4 t '(a b))
; 2
; > (lookup.4 t '(a c))
; 3
; > (lookup.4 t '(b c))
; 4
; > (lookup.4 t '(b a))
; #f

; Exercise 3.26

(define (make-table.5)
  (define root '())

  (define (make-node key value) (cons (cons key value) (cons '() '())))
  (define (get-key node) (car (car node)))
  (define (get-value node) (cdr (car node)))
  (define (left-child node) (car (cdr node)))
  (define (right-child node) (cdr (cdr node)))
  (define (set-left-child! parent child) (set-car! (cdr parent) child))
  (define (set-right-child! parent child) (set-cdr! (cdr parent) child))

  (define (insert-rec! node key value)
    (cond ((= key (get-key node))
           (set-cdr! (car node) value))
          ((< key (get-key node))
             (if (null? (left-child node))
                 (set-left-child! node (make-node key value))
                 (insert-rec! (left-child node) key value)))
            ((> key (get-key node))
             (if (null? (right-child node))
                 (set-right-child! node (make-node key value))
                 (insert-rec! (right-child node) key value)))))

  (define (insert! key value)
    (if (null? root)
        (set! root (make-node key value))
        (insert-rec! root key value))
    'ok)

  (define (lookup-rec node key)
    (cond ((null? node) false)
          ((= key (get-key node)) (get-value node))
          ((< key (get-key node)) (lookup-rec (left-child node) key))
          ((> key (get-key node)) (lookup-rec (right-child node) key))))

  (define (lookup key) (lookup-rec root key))

  (define (dispatch m)
    (cond ((eq? m 'lookup) lookup)
          ((eq? m 'insert!) insert!)
          ((eq? m 'debug) root)
          (else (error "Unknown operation -- MAKE-TABLE.5" m))))

  dispatch)

(define (lookup.5 table key) ((table 'lookup) key))
(define (insert.5! table key value) ((table 'insert!) key value))
(define (debug.5 table) (table 'debug))

; > (define t (make-table.5))
; > (debug.5 t)
; ()
; > (lookup.5 t 5)
; #f
; > (insert.5! t 3 'a)
; ok
; > (lookup.5 t 5)
; #f
; > (debug.5 t)
; ((3 . a) ())
; > (insert.5! t 1 'b)
; ok
; > (lookup.5 t 5)
; #f
; > (lookup.5 t 1)
; b
; > (debug.5 t)
; ((3 . a) ((1 . b) ()))
; > (insert.5! t 5 'c)
; ok
; > (debug.5 t)
; ((3 . a) ((1 . b) ()) (5 . c) ())
; > (lookup.5 t 5)
; c

; Chapter 3.3.4

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        ((and (= s1 0) (= s2 1)) 0)
        ((and (= s1 1) (= s2 0)) 0)
        ((and (= s1 1) (= s2 1)) 1)
        (else (error "Invalid signals" s1 s2))))

(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 1) (= s2 1)) 1)
        (else (error "Invalid signals" s1 s2))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))

  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; Exercise 3.28

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))

  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

; Exercise 3.29

(define (or-gate.2 a1 a2 output)
  (let ((b (make-wire))
        (c (make-wire))
        (d (make-wire)))
    (inverter a1 b)
    (inverter a2 c)
    (and-gate b c d)
    (inverter d output)
    'ok))

; ----------------------------------------

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))

    (define (call-each procedures)
      (if (null? procedures)
          'done
          (begin
            ((car procedures))
            (call-each (cdr procedures)))))
    
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- MAKE-WIRE" m))))

    dispatch))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire))
                 (newline))))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Plan is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

; > (probe 'sum sum)
; sum 0  New-value = 0
; > (probe 'carry carry)
; carry 0  New-value = 0

; > (half-adder input-1 input-2 sum carry)
; ok

; > (set-signal! input-1 1)
; done

; > (propagate)
; sum 8  New-value = 1
; done

; > (set-signal! input-2 1)
; done

; > (propagate)
; carry 11  New-value = 1
; sum 16  New-value = 0
; done

; Chapter 3.3.5

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe.2 name connector)
  (define (print-probe value)
    (display "Tester: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request -- PROBE.2" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Value is set" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?) (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown request -- CONNECTOR" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

; (define C (make-connector))
; (define F (make-connector))
; (celsius-fahrenheit-converter C F)

; (probe.2 "Celsius" C)
; (probe.2 "Fahrenheit" F)

; (set-value! C 25 'user)
; (forget-value! C 'user)

; (set-value! F 212 'user)

; Exercise 3.33

(define (average-converter a b c)
  (let ((w (make-connector))
        (u (make-connector)))
    (constant 2 w)
    (multiplier c w u)
    (adder a b u)
    'ok))

; > (define A (make-connector))
; > (define B (make-connector))
; > (define C (make-connector))

; > (average-converter A B C)

; > (probe.2 "First" A)
; > (probe.2 "Second" B)
; > (probe.2 "Average" C)

; > (set-value! A 10 'user)
; Tester: First = 10
; done

; > (set-value! B 20 'user)
; Tester: Second = 20
; Tester: Average = 15
; done

; > (forget-value! B 'user)
; Tester: Second = ?
; Tester: Average = ?
; done

; > (set-value! C 50 'user)
; Tester: Average = 50
; Tester: Second = 90
; done

; Exercise 3.35

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "Square less than 0 -- SQUARER" (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (if (has-value? a)
            (set-value! b
                        (* (get-value a) (get-value a))
                        me))))
  (define (process-forget-value)
    (forget-value! b me)
    (forget-value! a me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

; (define A (make-connector))
; (define B (make-connector))
; (probe.2 "A" A)
; (probe.2 "B" B)
; (squarer A B)
; (set-value! A 10 'user)
; (forget-value! A 'user)
; (set-value! B 64 'user)
; Tester: B = 100
; Tester: A = 10
; done
; Tester: B = ?
; Tester: A = ?
; done
; Tester: A = 8
; Tester: B = 64
; done

; Exercise 3.37

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- z x)
  (let ((y (make-connector)))
    (adder x y z)
    y))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ z x)
  (let ((y (make-connector)))
    (multiplier x y z)
    y))

(define (cv value)
  (let ((x (make-connector)))
    (constant value x)
    x))

(define (celsius-fahrenheit-converter.2 x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

; (define C (make-connector))
; (define F (celsius-fahrenheit-converter.2 C))

; (probe.2 "C" C)
; (probe.2 "F" F)

; (set-value! C 25 'user)

; Tester: C = 25
; Tester: F = 77
