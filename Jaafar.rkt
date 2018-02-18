(require "simpleParser.scm")

(define M_state_list
  (lambda (stmt-lis s)
    (cond
      ((null? statement-list) s)
      (else (M_state_list (cdr stmt-lis) (M_state (car stmt_lis s)))))))

(define M_state
  (lambda (stmt s)
    (cond
      ((null? stmt) s)
      ((eq? (operator stmt) 'var) (add (M_name stmt)))
      ((eq? (operator stmt) 'return) (M_value (return stmt) s))
      ((eq? (operator stmt) 'while) (M_state_while (condition stmt) (body stmt) s))
      (else s))))


; M_value
(define M_value
  (lambda (stmt s)
    (cond
      ((number? stmt) stmt)
      ((list? stmt) 


; M_boolean takes in a conditional statement and a state and returns true if the statement is true, and false otherwhise
(define M_boolean
  (lambda (statement s)
    s))


; M_state_if takes in a conditional statement and 2 outcomes, a then and an else, and a state and returns the appropriate state
(define M_state_if
  (lambda (condition then-statement else-statement s)
    (if (M_boolean condition s)
        (M_state then-statement (M_state condition s))
        (M_state else-statement (M_state condition s)))))




; M_state_while takes in a conditional, a body-statement, and a state and returns a state
(define M_state_while
  (lambda (condition body-statement s)
    (if (M_boolean condition s)
        (M_state_while condition body-statement (M_state body-statement (M_state condition s)))
        s)))


; Add: takes in a varriable, a value, and a state and adds the varriable with the given value to the state


; removevar: takes a state and a var and removes all instances of the var from the state
    

;=================================================
; Abstractions
;=================================================

; Operator: takes in a statement and returns the operator
(define operator
  (lambda (stmt)
    (cond
      ((null? stmt) (error 'thereIsNoStatement))
      ((list? stmt) (car stmt))
      (else (error 'invalidStatement)))))

; condition: take in a statement containing a condition and returns the condition
(define condition
  (lambda (stmt)
    (cadr stmt)))

(define body
  (lambda (stmt)
    (caddr stmt)))

(define then
  (lambda (stmt)
    (caddr stmt)))

(define else
  (lambda (stmt)
    (cadddr stmt)))

; Checks whether or not there is an else statement









(define callcc call-with-current-continuation)

; indexof - returns the index of an atom in a list (using index 0), -1 if the element is not in the list
(define indexof
  (lambda (x lis)
    (indexof-acc x lis 0)))

(define indexof-acc
  (lambda (x lis acc)
    (cond
      ((null? lis) -1)
      ((eq? x (car lis)) acc)
      (else (indexof-acc x (cdr lis) (+ 1 acc))))))


