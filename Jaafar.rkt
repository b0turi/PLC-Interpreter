(require "simpleParser.scm")

; interpret takes a filename and returns a value from return
(define interpret
  (lambda (filename)
    (ret_lookup (M_state_list (parser filename) '(('return)('null)))))) ; first state passed is the empty state with only the special return varrible with no assigned value (null)

; ret_lookup
(define ret_lookup
  (lambda (s)
    (lookup 'return s)))

; M_state_list takes in a statement list stmt-lis and a state s and returns a state 
(define M_state_list
  (lambda (stmt-lis s)
    (cond
      ((null? stmt-lis) s)
      (else (M_state_list (next stmt-lis) (M_state (first stmt_lis s)))))))

; M_state takes a statement and a state and retuns the state after the statment is evaluated
(define M_state
  (lambda (stmt s)
    (cond
      ((null? stmt) s)
      ((eq? (operator stmt) 'return) (M_assign 'return (M_value (return stmt) s) s))
      ((eq? (operator stmt) 'var) (add (var-name stmt) (M_value (assignment stmt)) s)
      ((eq? (operator stmt) 'while) (M_state_while (condition stmt) (body stmt) s))
      (else s)))))


; M_value
(define M_value
  (lambda (stmt s)
    (cond
      ((exp? stmt) (M_evaluate stmt s))
      ((number? stmt) stmt)
      ((var? stmt) (lookup stmt s))
      (else (error 'invalidValue)))))

; M_evaluate

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
(define add
  (lambda (varname value s)
    (cond
      ((eq? varname 'return) (error 'nameReserved))
      (( lookup)))))

; M_assign
(define M_assign
  (lambda 

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

; condition: take in an if or while statement containing a condition and returns the condition
(define condition
  (lambda (stmt)
    (cadr stmt)))

; body: takes in a while statement and returns the body of that statement
(define body
  (lambda (stmt)
    (caddr stmt)))


(define then
  (lambda (stmt)
    (caddr stmt)))

(define else
  (lambda (stmt)
    (cadddr stmt)))

(define next
  (lambda (lis)
    (cdr lis)))

(define var-name
  (lambda (stmt)
    (cadr stmt)))


(define assignment
  (lambda (stmt)
    (if (null? (cddr stmt))
        'null
        (caddr stmt))))

; Checks whether or not there is an else statement


; exp?: is the value an expression
(define exp? (lambda (v) (list? v)))







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


