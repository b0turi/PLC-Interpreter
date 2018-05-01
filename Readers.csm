
; ==========================================
; Abstractions, Part 3
; EECS 345 - Programming Language Concepts
;
; Group 23
; Jaafar Bennani
; Alex Hemm
; Kyle Thompson
; ==========================================

#lang racket
(provide (all-defined-out))

; ===== Function "readers" =====

; exp?
; Given an atom, return a boolean value as to whether the given atom is a pair, or, can be
; interpreted as an expression
(define exp?
  (lambda (v)
    (pair? v)))

; function-arguments
; Given a statement that is assumed to be a function call, retrieve the list of arguments to call the function with
(define function-arguments
  (lambda (stmt)
    (cddr stmt)))

; function-body
; Given a statement that is assumed to be a function declaration, retrieve the body of the function
(define function-body
  (lambda (stmt)
    (cadddr stmt)))

; function-name
; Given a statement that is assumed to be a function declaration, retrieve the name of the function
(define function-name
  (lambda (stmt)
    (cadr stmt)))

; function-parameters
; Given a statement that is assumed to be a function declaration, retrieve the list of parameters for the function
(define function-parameters
  (lambda (stmt)
    (caddr stmt)))

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

; try-body
; Given a statement known to be a try/catch block, retrieve the body of the try block
(define try-body
  (lambda (stmt)
    (cadr stmt)))

; catch?
; Returns whether a given statement is a catch block
(define catch?
  (lambda (stmt)
    (cond
      ((null? (caddr stmt)) #f)
      ((eq? (operator (caddr stmt)) 'catch) #t)
      (else #f))))

; catch-var
; As a part of error handling, grab the value to be returned within a catch block from a complete try/catch statement
(define catch-var
  (lambda (stmt)
    (caadr (caddr stmt))))

; catch-body
; Given a statement known to be a try/catch block, retrieve the body of the catch block
(define catch-body
  (lambda (stmt)
    (caddr (caddr stmt))))

; finally?
; Returns whether a given statement is a finally block
(define finally?
  (lambda (stmt)
    (cond
      ((null? (cadddr stmt)) #f)
      ((eq? (operator (cadddr stmt)) 'finally) #t)
      (else #f))))
; finally-body
; Given a statement that is known to be a try/catch/finally block, retrieve the body of the associated finally block
(define finally-body
  (lambda (stmt)
    (cadr (cadddr stmt))))

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
; Given a statment that is known to be a block of code, retrieve the body of the block
(define block-body
  (lambda (stmt)
    (cdr stmt)))

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

; ===== Miscellaneous =====

; first
(define first
  (lambda (lis)
    (car lis)))

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

(define returnvalue (lambda (v) v))