(require "simpleParser.scm")
(require "Abstractions.scm")

; interpret takes a filename and returns a value from return
(define interpret
  (lambda (filename)
    (lookup 'return (M_state_list (parser filename) '((return)(null)))))) ; first state passed is the empty state with only the special return varrible with no assigned value (null)

; M_state_list takes in a statement list stmt-lis and a state s and returns a state 
(define M_state_list
  (lambda (stmt-lis s)
    (cond
      ((null? stmt-lis) s)
      (else (M_state_list (next stmt-lis) (M_state (first stmt-lis) s))))))

; M_state takes a statement and a state and retuns the state after the statment is evaluated
(define M_state
  (lambda (stmt s)
    (cond
      ((null? stmt) s)
      ((not (exp? stmt)) s)
      ((eq? (operator stmt) 'return) (M_assign 'return (M_value (return stmt) s) (M_state (return stmt) s)))
      ((eq? (operator stmt) 'var) (add (var-name stmt) (M_value (assignment stmt) (M_state (assignment stmt) s)) (M_state (assignment stmt) s)))
      ((eq? (operator stmt) 'while) (M_state_while (condition stmt) (body stmt) s))
      ((eq? (operator stmt) 'if) (M_state_if (condition stmt) (then stmt) (else stmt) s))
      ((eq? (operator stmt) '=) (M_assign (var-name stmt) (M_value (assignment stmt) (M_state (assignment stmt) s)) (M_state (assignment stmt) s)))
      ((unary? stmt) (M_state (operand1 stmt) s))
      ((eq? (operator stmt) '!) (M_state (operand1 stmt) s))
      ((math_operator? (operator stmt)) (M_state (operand2 stmt) (M_state (operand1 stmt) s)))
      ((comp_operator? (operator stmt)) (M_state (operand2 stmt) (M_state (operand1 stmt) s)))
      ((bool_operator? (operator stmt)) (M_state (operand2 stmt) (M_state (operand1 stmt) s)))
      (else s))))


; M_value
(define M_value
  (lambda (stmt s)
    (cond
      ((exp? stmt) (M_evaluate stmt s))
      ((eq? stmt 'null) 'null)
      ((number? stmt) stmt)
      ((var? stmt s) (lookup stmt s))
      (else (error 'invalidValue)))))

; M_boolean takes in a conditional statement and a state and returns true if the statement is true, and false otherwhise
(define M_boolean
  (lambda (stmt s)
    (cond
      ((exp? stmt) (M_evaluate stmt s))
      ((boolean? stmt) stmt)
      (else (error 'invalidBoolean)))))

; M_evaluate
(define M_evaluate
  (lambda (exp s)
    (cond
      ((unary? exp) (M_value (* (M_value (operand1 exp) s) -1) s))   
      ((eq? (operator exp) '+) (M_value (+ (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s))) s))
      ((eq? (operator exp) '-) (M_value (- (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s))) s))
      ((eq? (operator exp) '*) (M_value (* (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s))) s))
      ((eq? (operator exp) '/) (M_value (quotient (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s))) s))
      ((eq? (operator exp) '%) (M_value (remainder (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s))) s))
      ((eq? (operator exp) '==) (M_boolean (= (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s))) s))
      ((eq? (operator exp) '!=) (M_boolean (not (= (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s)))) s))
      ((eq? (operator exp) '<) (M_boolean (< (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s))) s))
      ((eq? (operator exp) '>) (M_boolean (> (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s))) s))
      ((eq? (operator exp) '<=) (M_boolean (<= (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s))) s))
      ((eq? (operator exp) '>=) (M_boolean (>= (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s))) s))
      ((eq? (operator exp) '>=) (M_boolean (>= (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s))) s))
      ((eq? (operator exp) '&&) (M_boolean (and (M_boolean (operand1 exp) s) (M_boolean (operand2 exp) (M_state (operand1 exp) s))) s))
      ((eq? (operator exp) '||) (M_boolean (or (M_boolean (operand1 exp) s) (M_boolean (operand2 exp) (M_state (operand1 exp) s))) s))
      ((eq? (operator exp) '!) (M_boolean (not (M_boolean (operand1 exp) s)) s)))))

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
      (else (error 'variableAlreadyExists)))))

; M_assign takes a varriable value and state and returns the state with the varriable is assigned the value given 
    
(define M_assign
  (lambda (varname value s)
    (replace-value varname value s)))
        

