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
      ((eq? (operator stmt) 'return) (M_assign 'return (M_value (return stmt) s) s))
      ((eq? (operator stmt) 'var) (add (var-name stmt) (M_value (assignment stmt) (M_state (assignment stmt) s))))
      ((eq? (operator stmt) 'while) (M_state_while (condition stmt) (body stmt) s))
      (else s))))


; M_value
(define M_value
  (lambda (stmt s)
    (cond
      ((exp? stmt) (M_evaluate stmt s))
      ((number? stmt) stmt)
      ((var? stmt) (lookup stmt s))
      (else (error 'invalidValue)))))

; M_evaluate
(define M_evaluate
  (lambda (exp s)
    (cond
      ((eq? (operator stmt) '+) (M_value (+ (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '-) (M_value (- (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '*) (M_value (* (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '/) (M_value (quotient (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '%) (M_value (remainder (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '==) (M_boolean (= (M_boolean (operand1 exp) s) (M_boolean (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '!=) (M_boolean (not (M_boolean (operand1 exp) s) (M_boolean (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '<) (M_boolean (< (M_boolean (operand1 exp) s) (M_boolean (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '>) (M_boolean (> (M_boolean (operand1 exp) s) (M_boolean (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '<=) (M_boolean (<= (M_boolean (operand1 exp) s) (M_boolean (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '>=) (M_boolean (>= (M_boolean (operand1 exp) s) (M_boolean (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '>=) (M_boolean (>= (M_boolean (operand1 exp) s) (M_boolean (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '&&) (M_boolean (and (M_boolean (operand1 exp) s) (M_boolean (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '||) (M_boolean (or (M_boolean (operand1 exp) s) (M_boolean (operand2 exp) (M_state (operand1 exp) s)))))
      ((eq? (operator stmt) '!) (M_boolean (not (M_boolean (operand1 exp) s)))))))

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


; Add: takes in a varriable, a value, and a state , checks if the varriable has already beed declared, and adds the varriable to the state with the value given
(define add
  (lambda (varname value s)
    (cond
      ((eq? varname 'return) (error 'nameReserved))
      ((eq? (lookup varname s) 'varNotFound) (insert-var varname value s))
      (else (error 'varialeAlreadyExists)))))

; M_assign takes a varriable value and state and returns the state with the varriable is assigned the value given 
    
(define M_assign
  (lambda (varname value s)
    (if (eq? (lookup varname s) value)
        s
        (replace-value varname value s))))

; lookup - given a variable name and a state, check if that variable is defined in that state. Return its value or 'varNotFound if the variable doesn't exist in the state
(define lookup
  (lambda (varname s)
    (cond
      ((null? (next-varname s)) 'varNotFound)
      ((eq? varname (next-varname s)) (next-varvalue s))
      (else (lookup varname (pop-state s))))))
        
      
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

; next-varname: given a state s, get the name of the next variable in the state
(define next-varname
  (lambda (s)
    (if (null? (car s))
        '()
        (caar s))))

; next-varvalue: given a state s, get the value of the next variable in the state
(define next-varvalue
  (lambda (s)
    (if (null? (car s))
        '()
        (cadr s))))

; pop-state: given a state s, return the state with the first element of the two sublists removed
(define pop-state
  (lambda (s)
    (list (cdar s) (cdadr s))))

; insert-state: add a variable with a given varname and value to a given state s, and return the new state
(define insert-state
  (lambda (varname value s)
    (list (cons varname (cdar s)) (cons value (cdadr s)))))

; replace-value: given a variable name, value, and state, find the location within the state where the given variable name is stored and replace its value, and return the new state
(define replace-value
  (lambda (varname value s)
    (cond
      ((null? (next-varname s)) (error 'varNotFound))
      ((eq? varname (next-varname s)) (list (car s) (cons value (cdadr s))))
      (else (replace-value varname value (pop-state s)))

(define assignment
  (lambda (stmt)
    (if (null? (cddr stmt))
        'null
        (caddr stmt))))

    ; Checks if
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

; check if a value is the atom 'null
(define null_value?
  (lambda (x)
    (if (eq? 'null x)
        #t
        #f)))

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


