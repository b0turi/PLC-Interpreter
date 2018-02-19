#lang racket
(provide (all-defined-out))


;=================================================
; Abstractions
;=================================================

; lookup - given a variable name and a state, check if that variable is defined in that state. Return its value or 'varNotFound if the variable doesn't exist in the state
(define lookup
  (lambda (varname s)
    (cond
      ((emptystate? s) 'varNotFound)
      ((eq? varname (next-varname s)) (next-varvalue s))
      (else (lookup varname (pop-state s))))))

(define return (lambda (v) (cadr v)))

(define emptystate? (lambda (s) (equal? '(()()) s)))

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

(define var? (lambda (v s) (not (eq? (lookup v s) 'varNotFound))))

(define var-name
  (lambda (stmt)
    (cadr stmt)))

; next-varname: given a state s, get the name of the next variable in the state
(define next-varname
  (lambda (s)
    (if (null? (car s))
        '()
        (caar s))))

; next-varvalue: given a state s, get the value of the next variable in the state
(define next-varvalue
  (lambda (s)
    (if (null? (cadr s))
        '()
        (caadr s))))

; pop-state: given a state s, return the state with the first element of the two sublists removed
(define pop-state
  (lambda (s)
    (list (cdar s) (cdadr s))))

; insert-state: add a variable with a given varname and value to a given state s, and return the new state
(define insert-var
  (lambda (varname value s)
    (list (cons varname (car s)) (cons value (cadr s)))))

; replace-value: given a variable name, value, and state, find the location within the state where the given variable name is stored and replace its value, and return the new state
(define replace-value
  (lambda (varname value s)
    (cond
      ((emptystate? s) (error 'varNotFound))
      ((equal? varname (next-varname s)) (list (car s) (cons value (cdadr s))))
      (else (replace-value varname value (pop-state s))))))

(define assignment
  (lambda (stmt)
    (if (null? (cddr stmt))
        'null
        (caddr stmt))))

    ; Checks if
(define math_operator?
  (lambda (op)
    (if (null? op)
        (error 'nothingFound)
        (or
         (eq? '+ op)
         (eq? '- op)
         (eq? '* op)
         (eq? '/ op)
         (eq? '% op)))))

(define comp_operator?
  (lambda (op)
    (if (null? op)
        (error 'nothingFound)
        (or
          (eq? '== op)
          (eq? '!= op)
          (eq? '< op)
          (eq? '> op)
          (eq? '<= op)
          (eq? '>= op)))))

(define bool_operator?
  (lambda (op)
    (if (null? op)
        (error 'nothingFound)
        (or
         (eq? '&& op)
         (eq? '|| op)
         (eq? '! op)))))

; check if a value is the atom 'null
(define null_value?
  (lambda (x)
    (if (eq? 'null x)
        #t
        #f)))

; Checks whether or not there is an else statement


; exp?: is the value an expression
(define exp? (lambda (v) (list? v)))
