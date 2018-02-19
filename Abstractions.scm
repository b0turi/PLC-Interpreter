#lang racket
(provide (all-defined-out))


;=================================================
; Abstractions
;=================================================

; lookup - given a variable name and a state, check if that variable is defined in that state. Return its value or 'varNotFound if the variable doesn't exist in the state
(define lookup
  (lambda (varname s)
    (lookup-cps varname (car s) (cadr s) (lambda (v) v))))

(define lookup-cps
  (lambda (name namelis valuelis return)
    (cond
      ((null? namelis) (error "using before declaring"))
      ((and (eq? name (car namelis)) (eq? 'null (car valuelis))) (error "using before assigning"))
      ((eq? name (car namelis)) (return (car valuelis)))
      (else (lookup-cps name (cdr namelis) (cdr valuelis) return)))))

(define bool-lookup
  (lambda (varname s)
    ((lambda (value)
      (cond
        ((eq? value 'true) #t)
        ((eq? value 'false) #f)
        (else value)))
     (lookup varname s))))

(define exist?
  (lambda (varname s)
    (exist-cps varname (car s) (lambda (v) v))))

(define exist-cps
  (lambda (name namelis return)
    (cond
      ((null? namelis) (return #f))
      ((eq? name (car namelis)) (return #t))
      (else (exist-cps name (cdr namelis) return)))))

(define return (lambda (v) (cadr v)))

; Operator: takes in a statement and returns the operator
(define operator
  (lambda (stmt)
    (cond
      ((null? stmt) (error 'thereIsNoStatement))
      ((list? stmt) (car stmt))
      (else (error 'invalidStatement)))))

(define operand1
  (lambda (math_stmt)
    (cadr math_stmt)))

(define operand2
  (lambda (math_stmt)
    (caddr math_stmt)))

; condition: take in an if or while statement containing a condition and returns the condition
(define condition
  (lambda (stmt)
    (cadr stmt)))

; body: takes in a while statement and returns the body of that statement
(define body
  (lambda (stmt)
    (caddr stmt)))

(define unary?
  (lambda (stmt)
    (and (equal? (car stmt) '-) (null? (cddr stmt)))))
;======== IF ========
(define then
  (lambda (stmt)
    (caddr stmt)))

(define else
  (lambda (stmt)
    (if (null? (cdddr stmt))
        '()
        (cadddr stmt))))

(define next
  (lambda (lis)
    (cdr lis)))

(define var-name
  (lambda (stmt)
    (cadr stmt)))

; insert-state: add a variable with a given varname and value to a given state s, and return the new state
(define insert-var
  (lambda (varname value s)
    (cond
      ((eq? value #t) (list (cons varname (car s)) (cons 'true (cadr s))))
      ((eq? value #f) (list (cons varname (car s)) (cons 'false (cadr s))))
      (else (list (cons varname (car s)) (cons value (cadr s)))))))

; replace-value: given a variable name, value, and state, find the location within the state where the given variable name is stored and replace its value, and return the new state
(define replace-value
  (lambda (varname value s)
    (cond
      ((eq? value #t) (replaceval-cps varname 'true (car s) (cadr s) (lambda (l1 l2) (list l1 l2))))
      ((eq? value #f) (replaceval-cps varname 'false (car s) (cadr s) (lambda (l1 l2) (list l1 l2))))
      (else (replaceval-cps varname value (car s) (cadr s) (lambda (l1 l2) (list l1 l2)))))))

(define replaceval-cps
  (lambda (varname value namelis valuelis return)
    (cond
      ((null? namelis) (error "using before declaring"))
      ((equal? varname (car namelis)) (return namelis (cons value (cdr valuelis))))
      (else (replaceval-cps varname value (cdr namelis) (cdr valuelis) (lambda (l1 l2) (return (cons (car namelis) l1) (cons (car valuelis) l2))))))))

(define assignment
  (lambda (stmt)
    (if (null? (cddr stmt))
        'null
        (caddr stmt))))


(define single_value?
  (lambda (stmt)
    (or
     (eq? '! (operator stmt))
     (unary? stmt))))

(define dual_value?
  (lambda (op)
    (or
     (eq? '+ op)
     (eq? '- op)
     (eq? '* op)
     (eq? '/ op)
     (eq? '% op)
     (eq? '== op)
     (eq? '!= op)
     (eq? '< op)
     (eq? '> op)
     (eq? '<= op)
     (eq? '>= op)
     (eq? '&& op)
     (eq? '|| op))))

; check if a value is the atom 'null
(define null_value?
  (lambda (x)
    (if (eq? 'null x)
        #t
        #f)))

; Checks whether or not there is an else statement


; exp?: is the value an expression
(define exp? (lambda (v) (list? v)))
