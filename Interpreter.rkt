
; ==========================================
; Interpreter, Part 2
; EECS 345 - Programming Language Concepts
;
; Group 8
; Jaafar Bennani
; Alex Hemm
; Kyle Thompson
; ==========================================

(require "simpleParser.scm")
(require "Abstractions.scm")

; interpret
; Given a filename of Java/C-like code, use simpleParser to parse the file and then get the value that block of code returns
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (return)
       ; The initial state is empty
       (M_state_list (parser filename) (initstate) (goto-setup 'return return initgoto))))))

; M_state_list
; Given a list of statements and a state, evaluate each line with M_state and return the state
; after each line has been evaluated
(define M_state_list
  (lambda (stmt-lis s goto)
    (cond
      ((null? stmt-lis) s)
      (else (M_state_list (next stmt-lis) (M_state (first stmt-lis) s  goto) goto)))))

; M_state
; Given a statement and a state, evaluate the statement and return the new state
(define M_state
  (lambda (stmt s goto)
    (cond
      ; Ensure that the statement is an expression that can be evaluated, ie returns the same state is the input is not an expression
      ((not (exp? stmt)) s)

      ((eq? (operator stmt) 'return) (goto 'return (realvalue (M_value (return-exp stmt) s))))

      ((eq? (operator stmt) 'throw) (goto 'throw (throws (realvalue (M_value (return-exp stmt) s)) s)))

      ((state-goto? (operator stmt)) (goto (operator stmt) s))

      ((eq? (operator stmt) 'try) (M_state_try stmt s goto))
      
      ; Check if the statement branches 
      ((eq? (operator stmt) 'while) (M_state_while-start (condition stmt) (body stmt) s goto))
      ((eq? (operator stmt) 'if) (M_state_if (condition stmt) (then stmt) (else stmt) s goto))

      ; Check if the statement is a block of code, defined by "begin" in the parser
      ((eq? (operator stmt) 'begin) (M_state_block (block-body stmt) s goto))
      
      (else (M_state_side_effect stmt s)))))

; M_state_side_effect
(define M_state_side_effect
  (lambda (stmt s)
    (cond
      ; Ensure that the statement is an expression that can be evaluated, ie returns the same state is the input is not an expression
      ((not (exp? stmt)) s)
      
      ; Check if the statement reassigns a value 
      ((eq? (operator stmt) '=) (M_state_assign (var-name stmt) (M_value (assignment stmt) s) (M_state_side_effect (assignment stmt) s)))
      
      ; Check if the statement creates a new variable
      ((eq? (operator stmt) 'var) (M_state_declare (var-name stmt) (M_value (assignment stmt) s) (M_state_side_effect (assignment stmt) s)))

      ; Check if the statement is another kind of expression
      ((single_value? stmt) (M_state_side_effect (operand1 stmt) s))
      ((dual_value? (operator stmt)) (M_state_side_effect (operand2 stmt) (M_state_side_effect (operand1 stmt) s)))
      (else s))))

; M_state_block
(define M_state_block
  (lambda (stmt-lis s goto)
    (remove_layer (M_state_list stmt-lis (add_layer s) (block_goto goto)))))

; M_state_assign
; Given a variable name, value, and a state, update the state so
; that the value of the variable of the given name is the given value
(define M_state_assign
  (lambda (varname value s)
    (replace-value varname value s)))

; M_state_declare
; takes in a varriable, a value, and a state , checks if the varriable has already beed declared, and adds the varriable to the state with the value given
(define M_state_declare
  (lambda (varname value s)
    (cond
      ((exist-top? varname s) (error "Redefining"))
      (else (insert-var varname value s)))))

; M_state_if
; Given a condition, its relevant then and else statements, and a state, return the 
; new state with the relevant statement evaluated, based on the condition
(define M_state_if
  (lambda (condition then-statement else-statement s goto)
    (if (M_boolean condition s)
      (M_state then-statement s goto)
      (M_state else-statement s goto))))

(define M_state_while-start
  (lambda (condition body-statement s goto)
    (call/cc
     (lambda (break)
       (M_state_while condition body-statement s (goto-setup 'break break goto))))))

; M_state_while
; Given a condition, body, and state, recursively update the state until the condition is met
(define M_state_while
  (lambda (condition body-statement s goto)
    (if (M_boolean condition s)
      (M_state_while condition body-statement (M_state-continue body-statement s goto) goto)
      s)))

(define M_state-continue
  (lambda (stmt s goto)
    (call/cc
     (lambda (continue)
       (M_state stmt s (goto-setup 'continue continue goto))))))

; M_state_try
(define M_state_try
  (lambda (stmt s goto)
    (cond
      ((and (catch? stmt) (finally? stmt)) (M_state_block (finally-body stmt) (M_state_try_with-catch stmt s goto) goto))
      ((catch? stmt) (M_state_try_with-catch stmt s goto))
      ((finally? stmt) (M_state_block (finally-body stmt) (M_state_try_without-catch (try-body stmt) s goto) goto))
      (else (M_state_try_without-catch (try-body stmt) s goto)))))

; M_state_try_without-catch
(define M_state_try_without-catch
  (lambda (stmt s goto)
    (call/cc
     (lambda (throw)
       (M_state_block stmt s (goto-setup 'throw (lambda (t) (throw (throw-state t))) goto))))))

; M_state_try_with-catch
(define M_state_try_with-catch
  (lambda (stmt s goto)
    (call/cc
     (lambda (throw)
       (M_state_block (try-body stmt) s (goto-setup 'throw (lambda (t) (throw (M_state_catch stmt (throw-value t) (remove_layer (throw-state t)) goto))) goto))))))

; M_state_catch
(define M_state_catch
  (lambda (stmt e s goto)
    (M_state_block (catch-body stmt) (M_state_declare (catch-var stmt) e s) goto)))

; M_value
; Given a statement and a state, retrieve the value returned by the statement
(define M_value
  (lambda (stmt s)
    (cond
      ((exp? stmt) (M_evaluate stmt s))
      ((null_value? stmt) (nullvalue))
      ((or (boolean? stmt) (number? stmt)) stmt)
      ((boolvalue? stmt) (boolvalue stmt))
      (else (lookup stmt s)))))

; M_boolean
; Given a boolean statement and a state, return the boolean value of the statement
(define M_boolean
  (lambda (b-stmt s)
    (cond
      ((exp? b-stmt) (M_evaluate b-stmt s))
      ((boolean? b-stmt) b-stmt)
      ((boolvalue? b-stmt) (boolvalue b-stmt))
      (else (lookup b-stmt s)))))

; M_evaluate
; Given an expression and a state, perform the necessary operations given by the expression and return the new state
(define M_evaluate
  (lambda (exp s)
    (cond
      ((unary-? exp) (- 0 (M_value (operand1 exp) s)))
      ((eq? (operator exp) '!) (not (M_boolean (operand1 exp) s)))
      ((eq? (operator exp) '=) (M_value (assignment exp) (M_state_side_effect (assignment exp) s)))
      ((value_op? (operator exp)) (operation (operator exp) (M_value (operand1 exp) s) (M_value (operand2 exp) (M_state_side_effect (operand1 exp) s))))
      ((bool_op? (operator exp)) (operation (operator exp) (M_boolean (operand1 exp) s) (M_boolean (operand2 exp) (M_state_side_effect (operand1 exp) s))))
      (else (error "Expression id not valid")))))

