; ==========================================
; Interpreter, Part 1 - Abstractions
; EECS 345 - Programming Language Concepts
;
; Group 8
; Jaafar Bennani
; Alex Hemm
; Kyle Thompson
; ==========================================

#lang racket
(provide (all-defined-out))


; ===== Lookup =====
; lookup
; given a variable name and a state, check if that variable is defined in that state.
; Throws and apropreate error if the variable doesn't exist in the state or is uninitialized
(define lookup
  (lambda (varname s)
    (lookup-cps varname (car s) (cadr s) (lambda (v) v))))

; tail recursive helper for lookup
(define lookup-cps
  (lambda (name namelis valuelis return)
    (cond
      ((null? namelis) (error "using before declaring"))
      ((and (eq? name (car namelis)) (null_value? (car valuelis))) (error "using before assigning"))
      ((eq? name (car namelis)) (return (car valuelis)))
      (else (lookup-cps name (cdr namelis) (cdr valuelis) return)))))

; lookup - except for boolean expressions, will convert from true to #t and false to #f
(define bool-lookup
  (lambda (varname s)
    ((lambda (value)
      (cond
        ((eq? value 'true) #t)
        ((eq? value 'false) #f)
        (else value)))
     (lookup varname s))))

; ===== Exist? =====
; exist?
; takes in a variable name and a list, returns 
(define exist?
  (lambda (varname s)
    (exist-cps varname (car s) (lambda (v) v))))

; tail recursive helper for exist?
(define exist-cps
  (lambda (name namelis return)
    (cond
      ((null? namelis) (return #f))
      ((eq? name (car namelis)) (return #t))
      (else (exist-cps name (cdr namelis) return)))))

; ===== Function "readers" =====

; return
; takes in a return statement and returns the expression to be returned
(define return
  (lambda (stmt)
    (cadr stmt)))

; operator
; takes in a statement and returns the operator
(define operator
  (lambda (stmt)
    (cond
      ((null? stmt) (error 'thereIsNoStatement))
      ((list? stmt) (car stmt))
      (else (error 'invalidStatement)))))

; operand1
; takes in a statement and returns the first operand
(define operand1
  (lambda (math_stmt)
    (cadr math_stmt)))

; operand2
; takes in a statement and returns the second operand
(define operand2
  (lambda (math_stmt)
    (caddr math_stmt)))

; condition
; take in an if or while statement containing a condition and returns the condition
(define condition
  (lambda (stmt)
    (cadr stmt)))

; body
; takes in a while statement and returns the body of that statement
(define body
  (lambda (stmt)
    (caddr stmt)))
; then
; takes in an if statement and returns the "then" expression
(define then
  (lambda (stmt)
    (caddr stmt)))

; else
; takes in an if statement and returns the "else" expression if there is one, otherwise it returns '()
(define else
  (lambda (stmt)
    (if (null? (cdddr stmt))
        '()
        (cadddr stmt))))

; next
; returns the "next list" ie the cdr of the list
(define next
  (lambda (lis)
    (cdr lis)))

; var-name
; retunrs the name of the variable in a variable declaration
(define var-name
  (lambda (stmt)
    (cadr stmt)))

; assignment
; returns the assignment expression for a variable declaration or assignment, 'null if there isn't one
(define assignment
  (lambda (stmt)
    (if (null? (cddr stmt))
        'null
        (caddr stmt))))

; ===== STATE =====

; insert-state
; add a variable with a given varname and value to a given state s, and return the new state
(define insert-var
  (lambda (varname value s)
    (cond
      ((eq? value #t) (list (cons varname (car s)) (cons 'true (cadr s))))
      ((eq? value #f) (list (cons varname (car s)) (cons 'false (cadr s))))
      (else (list (cons varname (car s)) (cons value (cadr s)))))))

; replace-value
; given a variable name, value, and state, find the location within the state where the given variable name is stored and replace its value, and return the new state
(define replace-value
  (lambda (varname value s)
    (cond
      ((eq? value #t) (replaceval-cps varname 'true (car s) (cadr s) (lambda (l1 l2) (list l1 l2))))
      ((eq? value #f) (replaceval-cps varname 'false (car s) (cadr s) (lambda (l1 l2) (list l1 l2))))
      (else (replaceval-cps varname value (car s) (cadr s) (lambda (l1 l2) (list l1 l2)))))))

; tail recursive helper for replace-value
(define replaceval-cps
  (lambda (varname value namelis valuelis return)
    (cond
      ((null? namelis) (error "using before declaring"))
      ((equal? varname (car namelis)) (return namelis (cons value (cdr valuelis))))
      (else (replaceval-cps varname value (cdr namelis) (cdr valuelis) (lambda (l1 l2) (return (cons (car namelis) l1) (cons (car valuelis) l2))))))))

; ===== OPERATIONS =====

; unary-;
; takes in a statment, returns true if the operation is the unary -
(define unary-?
  (lambda (stmt)
    (and (equal? (car stmt) '-) (null? (cddr stmt)))))

; single_value?
; takes in a statement, returns true if the operation uses only one value
(define single_value?
  (lambda (stmt)
    (or (eq? '! (operator stmt)) (unary-? stmt))))

; dual_value?
; takes in an operator, returns true if the operator takes 2 values
(define dual_value?
  (lambda (op)
    (or (value_op? op) (bool_op? op))))

; value_op?
; takes in an operator, returns true if the operator operates on two numbers
(define value_op?
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
     (eq? '>= op))))

; bool_op?
; takes in an operator, returns true if the operator operates on two booleans
(define bool_op?
  (lambda (op)
    (or (eq? '&& op) (eq? '|| op))))

; operation
; takes in a dual value and two values, returns the appropreate result for the operation given 
(define operation
  (lambda (op v1 v2)
    (cond
      ((eq? '+ op) (+ v1 v2))
      ((eq? '- op) (- v1 v2))
      ((eq? '* op) (* v1 v2))
      ((eq? '/ op) (quotient v1 v2))
      ((eq? '% op) (remainder v1 v2))
      ((eq? '== op) (= v1 v2))
      ((eq? '!= op) (not (= v1 v2)))
      ((eq? '< op) (< v1 v2))
      ((eq? '> op) (> v1 v2))
      ((eq? '<= op) (<= v1 v2))
      ((eq? '>= op) (>= v1 v2))
      ((eq? '&& op) (and v1 v2))
      ((eq? '|| op) (or v1 v2))
      (else (error "Operator not valid")))))
      
  

; check if a value is the atom 'null
(define null_value?
  (lambda (x)
    (eq? 'null x)))

; exp?: is the value an expression
(define exp? (lambda (v) (pair? v)))
