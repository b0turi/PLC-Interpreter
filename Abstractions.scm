; ==========================================
; Abstractions
; ==========================================

#lang racket
(provide (all-defined-out))

; ===== Function "readers" =====

; return-exp
; Given a statement that is known to be a return statement,
; return the value the statement dictates is to be returned
(define return-exp
  (lambda (stmt)
    (cadr stmt)))

; operator
; Given a statement, retrieve the operator in the statement
(define operator
  (lambda (stmt)
    (cond
      ((null? stmt) (error 'thereIsNoStatement))
      ((list? stmt) (car stmt))
      (else (error 'invalidStatement)))))

; operand1
; Given a statement, retrieve the first operand
(define operand1
  (lambda (math_stmt)
    (cadr math_stmt)))

; operand2
; Given a statement that is known to be a binary expression,
; retrieve the second operand
(define operand2
  (lambda (math_stmt)
    (caddr math_stmt)))

; ==== If and While Statement Helpers ====

; condition
; Given a statement that is known to be either an if statement or a while statement,
; retrieve the condition that will determine which substatement runs in an if statement,
; or whether the body will be executed again in a while statement
(define condition
  (lambda (stmt)
    (cadr stmt)))

; then
; Given a statement that is known to be an if statement,
; retrieve the substatement to run when the condition is true
(define then
  (lambda (stmt)
    (caddr stmt)))

; else
; Given a statement that is known to be an if statement, retrieve the substatement
; to run when the condition is false, or '() if there is no else
(define else
  (lambda (stmt)
    (if (null? (cdddr stmt))
        '()
        (cadddr stmt))))

; body
; Given a statement that is known to be a while statement, retrieve the body
(define body
  (lambda (stmt)
    (caddr stmt)))

; block-body
(define block-body
  (lambda (stmt)
    (cadr stmt)))

; ===== STATE =====

; insert-var
; Given a variable name, value, and a state, insert the variable with the given value into the state,
; accounting for boolean literals, and return the new state with the variable added
(define insert-var
  (lambda (varname value s)
    (list (cons varname (car s)) (cons (realvalue value) (cadr s)))))

; replace-value
; Given a variable name, value, and state, find the location within the state where the given variable name is stored and replace its value, and return the new state
(define replace-value
  (lambda (varname value s)
    (replaceval-cps varname (realvalue value) (car s) (cadr s) (lambda (l1 l2) (list l1 l2)))))

; tail recursive helper for replace-value
(define replaceval-cps
  (lambda (varname value namelis valuelis return)
    (cond
      ((null? namelis) (error "using before declaring"))
      ((equal? varname (car namelis)) (return namelis (cons value (cdr valuelis))))
      (else (replaceval-cps varname value (cdr namelis) (cdr valuelis) (lambda (l1 l2) (return (cons (car namelis) l1) (cons (car valuelis) l2))))))))

(define add-state
  (lambda (newstate s)
    (cons newstate s)))

(define pop-state
  (lambda (s)
    (if (null? (cdddr s))
        (error "No substates")
        (cdr s))))

; ===== Lookup =====
; lookup
; Given a variable name and a state, check if that variable is defined in that state.
; Throws and appropriate error if the variable doesn't exist in the state or is uninitialized
(define lookup
  (lambda (varname s)
    (lookup-cps varname (car s) (cadr s) (lambda (v) v))))

; lookup-cps
; A tail recursive helper for lookup
(define lookup-cps
  (lambda (name namelis valuelis return)
    (cond
      ((null? namelis) (error "using before declaring"))
      ((and (eq? name (car namelis)) (null_value? (car valuelis))) (error "using before assigning"))
      ((eq? name (car namelis)) (return (car valuelis)))
      (else (lookup-cps name (cdr namelis) (cdr valuelis) return)))))

; bool-lookup
; An extention of lookup to handle boolean values and ensure that boolean
; literals are returned where the predefined values of true and false are stored
(define bool-lookup
  (lambda (varname s)
    (boolvalue (lookup varname s))))

(define boolvalue
  (lambda (v)
    (cond
      ((eq? v (truevalue)) #t)
      ((eq? v (falsevalue)) #f)
      (else (error "invalid boolean")))))

(define boolvalue?
  (lambda (v)
    (or (eq? v (truevalue)) (eq? v (falsevalue)))))

; ===== Exist? =====
; exist?
; takes in a variable name and a list, returns 
(define exist?
  (lambda (varname s)
    (exist?-cps varname (car s) (lambda (v) v))))

; exist?-cps
; A tail recursive helper for exist?
(define exist?-cps
  (lambda (name namelis return)
    (cond
      ((null? namelis) (return #f))
      ((eq? name (car namelis)) (return #t))
      (else (exist?-cps name (cdr namelis) return)))))

; ===== Miscellaneous =====

; next
; Given a list, return the list without its first element,
; so the first element is the "next" element of the original list
(define next
  (lambda (lis)
    (cdr lis)))

; var-name
; Given a statement known to be the declaration of a variable,
; retrieve the name of the new variable
(define var-name
  (lambda (stmt)
    (cadr stmt)))

; assignment
; Given a statement known to be an assignment to a variable,
; retrieve the value that is to be assigned to the variable
(define assignment
  (lambda (stmt)
    (if (null? (cddr stmt))
        'null
        (caddr stmt))))

; realvalue
; Given a value, check if the value is a boolean literal, and return
; the equivalent atom for printing, or return the original value if not
(define realvalue
  (lambda (v)
    (cond
      ((eq? v #t) (truevalue))
      ((eq? v #f) (falsevalue))
      (else v))))

; ===== OPERATIONS =====

; unary-?
; Given a statement, return whether the statement is a unary expression with '- as the operator
; as a boolean value
(define unary-?
  (lambda (stmt)
    (and (equal? (car stmt) '-) (null? (cddr stmt)))))

; single_value?
; Given a statement, return a boolean value as to whether the statement has only one operand
(define single_value?
  (lambda (stmt)
    (or (eq? '! (operator stmt)) (unary-? stmt))))

; dual_value?
; Given a statement, return a boolean value as to whether the statement has two operands
(define dual_value?
  (lambda (op)
    (or (value_op? op) (bool_op? op))))

; value_op?
; Given an operator, return a boolean value as to whether the operator is a value operator,
; or, an operator that requires two operands
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
; Given an operator, return a boolean value as to whether the operator is a boolean operator,
; or, an operator that can only operate on two booleans
(define bool_op?
  (lambda (op)
    (or (eq? '&& op) (eq? '|| op))))

; operation
; Given an operator and two operands, return the result of the operation on the two operands
(define operation
  (lambda (op v1 v2)
    (cond
      ; Value operators
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

      ; Boolean operators
      ((eq? '&& op) (and v1 v2))
      ((eq? '|| op) (or v1 v2))
      (else (error "Operator not valid")))))

; null_value?
; Given a value, return a boolean value as to whether the value is the predefined null value
(define null_value?
  (lambda (v)
    (eq? v (nullvalue))))

; exp?
; Given an atom, return a boolean value as to whether the given atom is a pair, or, can be
; interpreted as an expression
(define exp?
  (lambda (v)
    (pair? v)))

; ===== Predefined values =====
(define nullvalue
  (lambda ()
    'null))

(define truevalue
  (lambda ()
    'true))

(define falsevalue
  (lambda ()
    'false))

(define initstate
  (lambda ()
    '(()())))

(define returnvalue (lambda (v) v))
