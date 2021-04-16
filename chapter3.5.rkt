#lang sicp

; Chapter 3.5.1

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(define (stream-map procedure stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (procedure (stream-car stream))
                   (stream-map procedure (stream-cdr stream)))))

(define (stream-for-each procedure stream)
  (if (stream-null? stream)
      'done
      (begin (procedure (stream-car stream))
             (stream-for-each procedure (stream-cdr stream)))))

(define (display-stream stream)
  (stream-for-each display-line stream))

(define (display-line value)
  (display value)
  (newline))

(define (prime? n)
  (define (square x) (* x x))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (= n (smallest-divisor n)))

; > (prime? 17)
; #t
; > (prime? 18)
; #f

(define (stream-filter predicate stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((predicate (stream-car stream)) (cons-stream (stream-car stream)
                                                      (stream-filter predicate
                                                                     (stream-cdr stream))))
        (else (stream-filter predicate (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

; > (stream-car
;    (stream-cdr
;     (stream-filter prime?
;                    (stream-enumerate-interval 10000 1000000))))
; 10009

; Exercise 3.50

(define (stream-map.2 procedure . arg-streams)
  (if (stream-null? (car arg-streams))
      the-empty-stream
      (cons-stream (apply procedure (map stream-car arg-streams))
                   (apply stream-map.2 (cons procedure (map stream-cdr arg-streams))))))

; > (define r (stream-map.2 + '(1 2 3) '(4 5 6) '(7 8 9)))
; > r
; (12 . #<promise>)
; > (stream-cdr r)
; (15 . #<promise>)
; > (stream-cdr (stream-cdr r))
; (18 . #<promise>)

; Exercise 3.51

(define (show x)
  (display-line x)
  x)

; > (define x (stream-map show (stream-enumerate-interval 0 10)))
; 0
; > (stream-ref x 5)
; 1
; 2
; 3
; 4
; 5
; 5
; > (stream-ref x 7)
; 6
; 7
; 7

; Exercise 3.52

; > (define sum 0)
; > (define (accum x)
;     (set! sum (+ x sum))
;     sum)
; > (define seq (stream-map accum (stream-enumerate-interval 1 20)))
; > (define y (stream-filter even? seq))
; > (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
; > (stream-ref y 7)
; 136
; > (display-stream z)
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210
; done

; Chapter 3.5.2

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

; > (stream-ref no-sevens 100)
; 117

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map.2 + s1 s2))

(define integers.2 (cons-stream 1 (add-streams ones integers)))

(define fibs.2 (cons-stream 0
                            (cons-stream 1
                                         (add-streams (stream-cdr fibs)
                                                      fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes.2
  (cons-stream
   2
   (stream-filter prime.2? (integers-starting-from 3))))

(define (prime.2? n)
  (define (square x) (* x x))
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes.2))

; Exercise 3.53

; > (define s (cons-stream 1 (add-streams s s)))
; > (stream-ref s 0)
; 1
; > (stream-ref s 1)
; 2
; > (stream-ref s 2)
; 4
; > (stream-ref s 3)
; 8

; Exercise 3.54

(define (mul-streams s1 s2)
  (stream-map.2 * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))

; > (stream-ref factorials 0)
; 1
; > (stream-ref factorials 1)
; 2
; > (stream-ref factorials 2)
; 6
; > (stream-ref factorials 3)
; 24

; Exercise 3.55

(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (stream-cdr stream) (partial-sums stream))))

; > (stream-ref (partial-sums integers) 0)
; 1
; > (stream-ref (partial-sums integers) 1)
; 3
; > (stream-ref (partial-sums integers) 2)
; 6
; > (stream-ref (partial-sums integers) 3)
; 10
; > (stream-ref (partial-sums integers) 4)
; 15

; Exercise 3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (merge (scale-stream S 2) (scale-stream S 3)) (scale-stream S 5))))

; > (stream-ref S 0)
; 1
; > (stream-ref S 1)
; 2
; > (stream-ref S 2)
; 3
; > (stream-ref S 3)
; 4
; > (stream-ref S 4)
; 5
; > (stream-ref S 5)
; 6
; > (stream-ref S 6)
; 8
; > (stream-ref S 7)
; 9
; > (stream-ref S 8)
; 10
; > (stream-ref S 9)
; 12

(define (take stream n)
  (if (or (stream-null? stream) (= n 0))
      the-empty-stream
      (cons-stream (stream-car stream) (take (stream-cdr stream) (- n 1)))))

; Exercise 3.58

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; > (display-stream (take (expand 1 7 10) 10))
; 1
; 4
; 2
; 8
; 5
; 7
; 1
; 4
; 2
; 8
; done
; > (/ 1.0 7)
; 0.14285714285714285

; > (display-stream (take (expand 3 8 10) 5))
; 3
; 7
; 5
; 0
; 0
; done
; > (/ 3.0 8)
; 0.375

; Exercises 3.59a

(define (integrate-series series)
  (stream-map.2 (lambda (a n) (/ a n)) series integers))

; Exercises 3.59a

(define exp-series (cons-stream 1 (integrate-series exp-series)))

; > (display-stream (take exp-series 10))
; 1
; 1
; 1/2
; 1/6
; 1/24
; 1/120
; 1/720
; 1/5040
; 1/40320
; 1/362880
; done

(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))

(define sine-series
  (cons-stream 0 (scale-stream (integrate-series cosine-series) -1)))

; > (display-stream (take cosine-series 10))
; 1
; 0
; 1/2
; 0
; 1/24
; 0
; 1/720
; 0
; 1/40320
; 0
; done

; > (display-stream (take sine-series 10))
; 0
; 1
; 0
; 1/6
; 0
; 1/120
; 0
; 1/5040
; 0
; 1/362880
; done

; Exercise 3.60

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                            (mul-series s1 (stream-cdr s2)))))

(define sine-square (mul-series sine-series sine-series))
(define cosine-square (mul-series cosine-series cosine-series))
(define sine-square+cosine-square (add-streams sine-square cosine-square))

; > (display-stream (take sine-square+cosine-square 5))
; 1
; 0
; 0
; 0
; 0

; Exercise 3.61

(define (invert-unit-series series)
  (cons-stream 1
               (scale-stream (mul-series (stream-cdr series) (invert-unit-series series))
                             -1)))

; Exercise 3.62

(define (div-series s1 s2)
  (cond ((= (stream-car s2) 0)
         (error "Constant member in s2 is 0"))
        ((= (stream-car s2) 1)
         (mul-series s1 (invert-unit-series s2)))
        (else (let ((scaled-s2 (scale-stream s2 (/ 1 (stream-car s2)))))
                (mul-series s1
                            (scale-stream (invert-unit-series scaled-s2)
                                          (stream-car s2)))))))

(define tan-series (div-series sine-series cosine-series))

; > (display-stream (take tan-series 10))
; 0
; -1
; 0
; -1/3
; 0
; -2/15
; 0
; -17/315
; 0
; -62/2835
; done

; Chapter 3.5.3

(define (sqrt-improve guess x)
  (define (average a b) (/ (+ a b) 2))
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

; > (display-stream (take (sqrt-stream 2) 6))
; 1.0
; 1.5
; 1.4166666666666665
; 1.4142156862745097
; 1.4142135623746899
; 1.414213562373095
; done

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

; > (display-stream (take pi-stream 10))
; 4.0
; 2.666666666666667
; 3.466666666666667
; 2.8952380952380956
; 3.3396825396825403
; 2.9760461760461765
; 3.2837384837384844
; 3.017071817071818
; 3.2523659347188767
; 3.0418396189294032
; done

(define (euler-transform s)
  (define (square x) (* x x))
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

; > (display-stream (take (euler-transform pi-stream) 10))
; 3.166666666666667
; 3.1333333333333337
; 3.1452380952380956
; 3.13968253968254
; 3.1427128427128435
; 3.1408813408813416
; 3.142071817071818
; 3.1412548236077655
; 3.1418396189294033
; 3.141406718496503
; done

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

; > (display-stream (take (accelerated-sequence euler-transform pi-stream) 10))
; 4.0
; 3.166666666666667
; 3.142105263157895
; 3.141599357319005
; 3.1415927140337785
; 3.1415926539752927
; 3.1415926535911765
; 3.141592653589778
; 3.1415926535897953
; 3.141592653589795
; done

; Exersice 3.64

(define (stream-limit series epsilon)
  (define (iter previous tail)
    (let ((current (stream-car tail)))
      (if (< (abs (- current previous)) epsilon)
          current
          (iter current (stream-cdr tail)))))
  (iter (stream-car series) (stream-cdr series)))

(define (sqrt.2 x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; > (sqrt.2 2 0.0000001)
; 1.414213562373095

; Exercise 3.65

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream (partial-sums (ln2-summands 1)))

; > (display-stream (take ln2-stream 10))
; 1.0
; 0.5
; 0.8333333333333333
; 0.5833333333333333
; 0.7833333333333332
; 0.6166666666666666
; 0.7595238095238095
; 0.6345238095238095
; 0.7456349206349207
; 0.6456349206349207
; done

; > (display-stream (take (euler-transform ln2-stream) 10))
; 0.7
; 0.6904761904761905
; 0.6944444444444444
; 0.6924242424242424
; 0.6935897435897436
; 0.6928571428571428
; 0.6933473389355742
; 0.6930033416875522
; 0.6932539682539683
; 0.6930657506744464
; done

; > (display-stream (take (accelerated-sequence euler-transform ln2-stream) 10))
; 1.0
; 0.7
; 0.6932773109243697
; 0.6931488693329254
; 0.6931471960735491
; 0.6931471806635636
; 0.6931471805604039
; 0.6931471805599445
; 0.6931471805599427
; 0.6931471805599454
; done

; ----------------------------------------

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

; > (display-stream (take (pairs integers integers) 10))
; (1 1)
; (1 2)
; (2 2)
; (1 3)
; (2 3)
; (1 4)
; (3 3)
; (1 5)
; (2 4)
; (1 6)
; done

; Exercise 3.67

(define (interleave.3 s1 s2 s3)
  (if (stream-null? s1)
      (if (stream-null? s2)
          s3
          (interleave s2 s3))
      (cons-stream (stream-car s1)
                   (interleave.3 s2 s3 (stream-cdr s1)))))

(define (pairs.2 s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave.3
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (stream-map (lambda (y) (list y (stream-car t)))
                (stream-cdr s))
    (pairs.2 (stream-cdr s) (stream-cdr t)))))

; > (display-stream (take (pairs.2 integers integers) 10))
; (1 1)
; (1 2)
; (2 1)
; (2 2)
; (1 3)
; (3 1)
; (2 3)
; (1 4)
; (4 1)
; (3 2)
; done

; Exercise 3.68

(define (pairs.3 s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs.2 (stream-cdr s) (stream-cdr t))))
