; ==========================================
; Interpreter, Part 1
; EECS 345 - Programming Language Concepts
;
; Group 8
; Jaafar Bennani
; Alex Hemm
; Kyle Thompson
; ==========================================

; Abstraction methods were placed in a separate file to improve readability
(require "Abstractions.scm")
(require "simpleParser.scm")

; interpret
; Given a filename of Java/C-like code, use simpleParser to parse the file and then get the value that block of code returns
(define interpret
  (lambda (filename)
     ; The initial state contains the special variable return with a default value of null
    (lookup 'return (M_state_list (parser filename) '((return) (null)))))) 

; M_state_list
; Given a list of statements and a state, evaluate each line with M_state and return the state
; after each line has been evaluated
(define M_state_list
  (lambda (stmt-lis s)
    (cond
      ((null? stmt-lis) s)
      (else (M_state_list (next stmt-lis) (M_state (first stmt-lis) s))))))

; M_state
; Given a statement and a state, evaluate the statement and return the new state
(define M_state
  (lambda (stmt s)
    (cond
      ; Ensure that the statement is an expression that can be evaluated, ie returns the same state is the input is not an expression
      ((not (exp? stmt)) s)

      ; Check if the statement reassigns or returns a value 
      ((eq? (operator stmt) 'return) (M_state_assign 'return (M_value (return stmt) s) (M_state (return stmt) s)))
      ((eq? (operator stmt) '=) (M_state_assign (var-name stmt) (M_value (assignment stmt) s) (M_state (assignment stmt) s)))
      
      ; Check if the statement branches 
      ((eq? (operator stmt) 'while) (M_state_while (condition stmt) (body stmt) s))
      ((eq? (operator stmt) 'if) (M_state_if (condition stmt) (then stmt) (else stmt) s))

      ; Check if the statement creates a new variable
      ((eq? (operator stmt) 'var) (M_state_declare (var-name stmt) (M_value (assignment stmt) s) (M_state (assignment stmt) s)))

      ; Check if the statement is another kind of expression
      ((single_value? stmt) (M_state (operand1 stmt) s))
      ((dual_value? (operator stmt)) (M_state (operand2 stmt) (M_state (operand1 stmt) s)))
      (else s))))


; M_value
; Given a statement and a state, retrieve the value returned by the statement
(define M_value
  (lambda (stmt s)
    (cond
      ((exp? stmt) (M_evaluate stmt s))
      ((eq? stmt 'null) 'null)
      ((number? stmt) stmt)
      (else (lookup stmt s)))))

; M_boolean
; Given a boolean statement and a state, return the boolean value of the statement
(define M_boolean
  (lambda (b-stmt s)
    (cond
      ((exp? b-stmt) (M_evaluate b-stmt s))
      ((boolean? b-stmt) b-stmt)
      ((eq? b-stmt 'true) #t)
      ((eq? b-stmt 'false) #f)
      (else (bool-lookup b-stmt s)))))

; M_evaluate
; Given an expression and a state, perform the necessary operations given by the expression and return the new state
(define M_evaluate
  (lambda (exp s)
    (cond
      ((unary-? exp) (- 0 (M_value (operand1 exp) s)))
      ((eq? (operator exp) '!) (not (M_boolean (operand1 exp) s)))
      ((eq? (operator exp) '=) (M_value (assignment exp) (M_state (assignment exp) s)))
      ((value_op? (operator exp)) (operation (operator exp) (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state (operand1 exp) s))))
      ((bool_op? (operator exp)) (operation (operator exp) (M_boolean (operand1 exp) s) (M_boolean (operand2 exp) (M_state (operand1 exp) s))))
      (else (error "Operator not valid")))))

; M_state_if
; Given a condition, its relevant then and else statements, and a state, return the 
; new state with the relevant statement evaluated, based on the condition
(define M_state_if
  (lambda (condition then-statement else-statement s)
    (if (M_boolean condition s)
      (M_state then-statement (M_state condition s))
      (M_state else-statement (M_state condition s)))))

; M_state_while
; Given a condition, body, and state, recursively update the state until the condition is met
(define M_state_while
  (lambda (condition body-statement s)
    (if (M_boolean condition s)
      (M_state_while condition body-statement (M_state body-statement (M_state condition s)))
      (M_state condition s))))

; M_state_assign
; Given a variable name, value, and a state, update the state so
; that the value of the variable of the given name is the given value
(define M_state_assign
  (lambda (varname value s)
    (replace-value varname value s)))


; Add: takes in a varriable, a value, and a state , checks if the varriable has already beed declared, and adds the varriable to the state with the value given
(define M_state_declare
  (lambda (varname value s)
    (cond
      ((eq? varname 'return) (error "Name Reserved"))
      ((exist? varname s) (error "Redefining"))
      (else (insert-var varname value s)))))