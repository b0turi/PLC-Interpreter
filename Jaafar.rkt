(require "simpleParser.scm")

; interpret takes a filename and returns a value from return
(define interpret
  (lambda (filename)
    (lookup 'return (M_state_list (parser filename) '(('return)('null)))))) ; first state passed is the empty state with only the special return varrible with no assigned value (null)

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
      ((eq? (operator stmt) 'return) (M_assign 'return (M_value (return stmt) s) (M_state (return stmt) s)))
      ((eq? (operator stmt) 'var) (add (var-name stmt) (M_value (assignment stmt) (M_state (assignment stmt) s))))
      ((eq? (operator stmt) 'while) (M_state_while (condition stmt) (body stmt) s))
      ((eq? (operator stmt) 'if) 'cry)
      ((eq? (operator stmt) '=) (M_assign (var-name stmt) (assignment stmt) (M_state (assignment stmt) s)))
      ((math_operator? (operator stmt)) (M_state (oprand2 stmt) (M_state (oprand1 stmt) s)))
      ((comp_operator? (operator stmt)) (M_state (oprand2 stmt) (M_state (oprand1 stmt) s)))
      ((bool_operator? (operator stmt)) (M_state (oprand2 stmt) (M_state (oprand1 stmt) s)))
      (else s)))))


; M_value
(define M_value
  (lambda (stmt s)
    (cond
      ((exp? stmt) (M_evaluate stmt s))
      ((number? stmt) stmt)
      ((var? stmt) (lookup stmt s))
      (else (error 'invalidValue)))))

; M_boolean takes in a conditional statement and a state and returns true if the statement is true, and false otherwhise
(define M_boolean
  (lambda (stmt s)
    (cond
      ((exp? stmt) (M_evaluate stmt s))
      (((boolean? stmt) stmt)
      (else (error 'invalidBoolean)))))

; M_evaluate
(define M_evaluate
  (lambda (exp s)
    (cond
      ((eq? (opperator stmt) '+) (M_value (+ (M_value (oprand1 exp) s) (M_value (oprand2 exp) (M_state (oprand1 exp) s))) s)
      ((eq? (opperator stmt) '-) (- (M_value (oprand1 exp) s) (M_value (oprand2 exp) (M_state (oprand1 exp) s))))
      ((eq? (opperator stmt) '*) (+ (M_value (oprand1 exp) s) (M_value (oprand2 exp) (M_state (oprand1 exp) s))))
      ((eq? (opperator stmt) '==) (= (M_value (oprand1 exp) s) (M_value (oprand2 exp) (M_state (oprand1 exp) s))))
      ((eq? (opperator stmt) '!=) (not (= (M_value (oprand1 exp) s) (M_value (oprand2 exp) (M_state (oprand1 exp) s)))))
      ((eq? (opperator stmt) '<) (< (M_value (oprand1 exp) s) (M_value (oprand2 exp) (M_state (oprand1 exp) s))))
      ((eq? (opperator stmt) '>) (> (M_value (oprand1 exp) s) (M_value (oprand2 exp) (M_state (oprand1 exp) s))))
      ((eq? (opperator stmt) '<=) (<= (M_value (oprand1 exp) s) (M_value (oprand2 exp) (M_state (oprand1 exp) s))))
      ((eq? (opperator stmt) '>=) (>= (M_value (oprand1 exp) s) (M_value (oprand2 exp) (M_state (oprand1 exp) s))))
      ((eq? (opperator stmt) '&&) (and (M_value (oprand1 exp) s) (M_value (oprand2 exp) (M_state (oprand1 exp) s))))
      ((eq? (opperator stmt) '||) (or (M_value (oprand1 exp) s) (M_value (oprand2 exp) (M_state (oprand1 exp) s))))
      ((eq? (opperator stmt) '!) (not (M_value (oprand1 exp) s)))



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


; Add: takes in a varriable, a value, and a state , checks if the varriable has already beed declared, and adds the varriable to the state with the value given
(define add
  (lambda (varname value s)
    (cond
      ((eq? varname 'return) (error 'nameReserved))
      ((lookup)))))

; M_assign takes a varriable value and state and returns the state with the varriable is assigned the value given 
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
    (if (null? (cdddr stmt))
        '()
        (cadddr stmt))))

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

(define math_operator?
  (lambda (op)
    (if
      ((null? op) (error 'nothing found))
      (or
       (eq? '+ (op))
       (eq? '- (op))
       (eq? '* (op))
       (eq? '/ (op))
       (eq? '% (op))))))

(define comp_operator?
  (lambda (op)
    (if
      (null? op) (error 'nothing found)
      (or
        (eq? '== (op))
        (eq? '!= (op))
        (eq? '< (op))
        (eq? '> (op))
        (eq? '<= (op))
        (eq? '>= (op))))))

(define bool_operator?
  (lambda (op)
    (if
     (null? op) (error 'nothing found)
     (or
      (eq? '&& (op))
      (eq? '|| (op))
      (eq? '! (op))))))

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


