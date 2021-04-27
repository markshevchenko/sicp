#lang sicp

; Chapter 4.1.1

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-assignment exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extent-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

; ----------------------------------------

(define (lookup-variable-value exp env) '())
(define (text-of-assignment exp env) '())
(define (make-procedure parameters body env) '())
(define (primitive-procedure? procedure) false)
(define (apply-primitive-procedure procedure arguments) '())
(define (compound-procedure? procedure) '())
(define (procedure-body procedure) '())
(define (extent-environment parameters arguments env) '())
(define (procedure-parameters procedure) '())
(define (procedure-environment procedure) '())
(define (true? value) value)
(define (false? value) (not value))
(define (set-variable-value! variable value env) '())
(define (define-variable! variable value env) '())

; ----------------------------------------

; Exercise 4.1

(define (list-of-values-ltr exps env)
  (if (no-operands? exps)
      '()
      (let ((first-operand-value (eval (first-operand exps) env))
            (rest-operands-value (list-of-values (rest-operands exps) env)))
        (cons first-operand-value rest-operands-value))))

(define (list-of-values-rtl exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-operands-value (list-of-values (rest-operands exps) env))
            (first-operand-value (eval (first-operand exps) env)))
        (cons first-operand-value rest-operands-value))))

; Chapter 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequence alternative)
  (list 'if predicate consequence alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE branch is not last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

; Exercise 4.3

(define (make-table)
  (define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  
  (let ((table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            false)))

    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (set-cdr! table
                      (cons (cons key value) (cdr table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- MAKE-TABLE" m))))

    dispatch))

(define eval-table (make-table))
(define get (eval-table 'lookup))
(define put! (eval-table 'insert!))

(put! 'quote text-of-assignment)
(put! 'set! eval-assignment)
(put! 'define eval-definition)
(put! 'if eval-if)
(put! 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
                                                (lambda-body exp)
                                                env)))

(put! 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put! 'cond (lambda (exp env) (eval.2 (cond->if exp) env)))

(define (eval.2 exp env)
  (cond ((symbol? (car exp))
         (let ((handler (get (car exp))))
           (handler exp env)))
        ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

; Exercise 4.4

(define (eval-and exp env)
  (define (iter conditions)
    (cond ((null? conditions) true)
          ((false? (eval (car conditions) env)) false)
          (else (iter (cdr conditions)))))

  (iter (cdr exp)))

(define (eval-or exp env)
  (define (iter conditions)
    (cond ((null? conditions) false)
          ((true? (eval (car conditions) env)) true)
          (else (iter (cdr conditions)))))

  (iter (cdr exp)))

; Exercise 4.5

(define (pointed-cond? rest) (eq? (car rest) '=>))

(define (expand-clauses.2 clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE branch is not last -- COND->IF" clauses))
            (if (pointed-cond? rest)
                (let ((procedure (cadr rest)))
                  (make-if (cond-predicate first)
                           (list procedure first)
                           (expand-clauses.2 rest)))
                (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses.2 rest)))))))

; Exercise 4.6

(define (let? exp) (tagged-list? exp 'let))

(define (let-parameters exp)
  (map car (cadr exp)))

(define (let-values exp)
  (map cadr (cadr exp)))

(define (let-body exp)
  (caddr exp))

(define (make-let parameters values body)
  (cons (list (make-lambda parameters body))
        values))

; Exercise 4.7

(define (let*->nested-lets exp)
  (define (iter parameter-pairs body)
    (if (null? parameter-pairs)
        body
        (let ((parameter (car (car parameter-pairs)))
              (value (cadr (car parameter-pairs))))
          (make-let (list parameter) (list value) (iter (cdr parameter-pairs) body)))))
  (let ((parameter-pairs (cadr exp))
        (body (caddr exp)))
    (iter parameter-pairs body)))
    
