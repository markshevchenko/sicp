#lang sicp

; Chapter 3.1.1

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Not enough money"))

; > (withdraw 20)
; 80
; > (withdraw 30)
; 50
; > (withdraw 55)
; "Not enough money"
; > (withdraw 50)
; 0


(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Not enough money"))))

; > (new-withdraw 0)
; 100
; > (new-withdraw 90)
; 10
; > (new-withdraw 20)
; "Not enough money"
; > (new-withdraw 10)
; 0

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Not enough money")))

; > (define w1 (make-withdraw 100))
; > (define w2 (make-withdraw 200))
; > (w1 50)
; 50
; > (w2 120)
; 80

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Not enough money"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown call -- MAKE-ACCOUNT" m))))

  dispatch)

; > (define acc (make-account 100))
; > ((acc 'withdraw) 50)
; 50
; > ((acc 'withdraw) 60)
; "Not enough money"
; > ((acc 'deposit) 40)
; 90
; > ((acc 'withdraw) 60)
; 30

; Exercise 3.1

(define (make-accumulator sum)
  (lambda (addendum)
    (begin (set! sum (+ sum addendum))
           sum)))

; > (define a (make-accumulator 5))
; > (a 10)
; 15
; > (a 10)
; 25

; Exercise 3.2

(define (make-monitored f)
  (define counter 0)

  (lambda (x)
    (cond ((eq? x 'how-many-calls?) counter)
          ((eq? x 'reset-count) (begin (set! counter 0)
                                       0))
          (else (begin (set! counter (inc counter))
                       (f x))))))

; > (define s (make-monitored sqrt))
; > (s 100)
; 10
; > (s 'how-many-calls?)
; 1
; > (s 3)
; 1.7320508075688772
; > (s 'how-many-calls?)
; 2
; > (s 'reset-count)
; 0
; > (s 200)
; 14.142135623730951
; > (s 'how-many-calls?)
; 1

; Exercise 3.3
; Exercise 3.4

(define (make-secure-account balance password)
  (define alarm-count 7)
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Not enough money"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch guess-password m)
    (if (eq? password guess-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown call -- MAKE-ACCOUNT" m)))
        (lambda (_)
          (set! alarm-count (dec alarm-count))
          (if (= alarm-count 0)
              (call-the-cops)
              "Invalid password"))))

  dispatch)

; > (define acc (make-secure-account 100 'god))
; > ((acc 'god 'withdraw) 40)
; 60
; > ((acc 'dog 'deposit) 50)
; "Invalid password"

(define (call-the-cops)
  "Police!")

; > (define acc (make-secure-account 100 'god))
; > ((acc 'dog 'withdraw) 1)
; "Invalid password"
; > ((acc 'dog 'withdraw) 2)
; "Invalid password"
; > ((acc 'dog 'withdraw) 3)
; "Invalid password"
; > ((acc 'dog 'withdraw) 4)
; "Invalid password"
; > ((acc 'dog 'withdraw) 5)
; "Invalid password"
; > ((acc 'dog 'withdraw) 6)
; "Invalid password"
; > ((acc 'dog 'withdraw) 7)
; "Police!"

; Chapter 3.1.2

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (random 100) (random 100)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))

  (iter trials 0))

; Exercise 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p xmin ymin xmax ymax trials)
  (define (round-test)
    (let ((x (random-in-range xmin xmax))
          (y (random-in-range ymin ymax)))
      (p x y)))

  (let ((coeff (monte-carlo trials round-test))
        (square (* (- xmax xmin) (- ymax ymin))))
    (* square coeff)))

(define (p x y)
  (let ((dx (- x 5))
        (dy (- y 7)))
    (<= (+ (* dx dx) (* dy dy)) (* 3 3))))

(define (p-pi x y)
  (<= (+ (* x x) (* y y)) 1))

; Exercise 3.6

(define seed 0)

(define (rand proc)
  (cond ((eq? proc 'generate) (begin (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648))
                                     seed))
        ((eq? proc 'reset) (lambda (new-seed)
                             (set! seed new-seed)))
        (else (error "Unknown proc -- RAND" proc))))