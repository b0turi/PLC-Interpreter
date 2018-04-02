
; ==========================================
; Interpreter, Part 3
; EECS 345 - Programming Language Concepts
;
; Group 8
; Jaafar Bennani
; Alex Hemm
; Kyle Thompson
; ==========================================

(require "functionParser.scm")
(require "Abstractions.scm")

; interpret
; Given a filename of Java/C-like code, use simpleParser to parse the file and then get the value that block of code returns
(define interpret
  (lambda (filename)
    ; The initial state is empty
    (M_state_main (M_state_list (parser filename) (initstate) initgoto) initgoto)))

; M_state_assign
; Given a variable name, value, and a state, update the state so
; that the value of the variable of the given name is the given value
(define M_state_assign
  (lambda (varname value s goto)
    (replace-value varname value s)))

; M_state_block
; Given a list of statements contained within curly braces {} in the original code, iterate through the block, executing each line,
; and storing a goto value to keep track of where the line of execution should go after the block is completed
(define M_state_block
  (lambda (stmt-lis s goto)
    ; Each block of code exists within its own layer on top of the state, which must be removed after the block is completed
    (remove_layer (M_state_list stmt-lis (add_layer s) (block_goto goto)))))

; M_state_declare
; takes in a varriable, a value, and a state , checks if the varriable has already beed declared, and adds the varriable to the state with the value given
(define M_state_declare
  (lambda (varname value s goto)
    (cond
      ((exist-top? varname s) (error "Redefining"))
      (else (insert-var varname value s)))))

; M_state_function
; Given a function name, a list of argument values, a state, and a goto continuation,
; return the function environment after the function has been executed
(define M_state_function
  (lambda (fname args s goto)
    (call/cc
     (lambda (return)
    (cond
      ((not (exist? fname s)) (error "Function does not exist"))
      (else (M_state_list (closure-body (lookup fname s)) (fsetup (closure-params (lookup fname s)) args (add_layer (remove-layer fname s))) (goto-setup 'return (gotoreturn return s) (func-goto goto)))))))))

; M_state_if
; Given a condition, its relevant then and else statements, and a state, return the 
; new state with the relevant statement evaluated, based on the condition
(define M_state_if
  (lambda (condition then-statement else-statement s goto)
    (if (M_boolean condition s goto)
        (M_state then-statement s goto)
        (M_state else-statement s goto))))

; M_state_list
; Given a list of statements and a state, evaluate each line with M_state and return the state
; after each line has been evaluated
(define M_state_list
  (lambda (stmt-lis s goto)
    (cond
      ((null? stmt-lis) s)
      (else (M_state_list (next stmt-lis) (M_state (first stmt-lis) s  goto) goto)))))

; M_state_main
; Given an environment and a goto continuation,
; search for the main function and call it using M_value_function, passing the goto continuation as well
(define M_state_main
  (lambda (s goto)
    (M_value_function 'main '() s goto)))
    
; M_state
; Given a statement and a state, evaluate the statement and return the new state
(define M_state
  (lambda (stmt s goto)
    (cond
      ; Ensure that the statement is an expression that can be evaluated, ie returns the same state is the input is not an expression
      ((not (exp? stmt)) s)
      
      ; Check if the statement creates a new variable
      ((eq? (operator stmt) 'var) (M_state_declare (var-name stmt) (M_value (assignment stmt) s goto) (M_state_side-effect (assignment stmt) s goto) goto))
      
      ; Check if the statement is a function declaration
      ((eq? (operator stmt) 'function) (M_state_declare (var-name stmt) (closure (function-parameters stmt) (function-body stmt)) s goto))

      ; Check if the statement is a branching statement with a goto
      ((eq? (operator stmt) 'return) (goto 'return (realvalue (M_value (return-exp stmt) (M_state_side-effect (assignment stmt) s goto) goto))))
      ((eq? (operator stmt) 'throw) (goto 'throw (throws (realvalue (M_value (return-exp stmt) s goto)) s)))
      ((state-goto? (operator stmt)) (goto (operator stmt) s))
      ((eq? (operator stmt) 'try) (M_state_try stmt s goto))
      
      ; Check if the statement is a branching statement with a condition
      ((eq? (operator stmt) 'while) (M_state_while-start (condition stmt) (body stmt) s goto))
      ((eq? (operator stmt) 'if) (M_state_if (condition stmt) (then stmt) (else stmt) s goto))

      ; Check if the statement is a block of code, defined by "begin" in the parser
      ((eq? (operator stmt) 'begin) (M_state_block (block-body stmt) s goto))
      
      ; Check if a function is being called
      ((eq? (operator stmt) 'funcall) (M_state_function (function-name stmt) (M_value_list (function-arguments stmt) s goto) s goto))
      
      (else (M_state_side-effect stmt s goto)))))

; M_state_side_effect
; Given a statement, a state, and a goto continuation,
; Evaluate the statement but also account for any side effects that may occur by recursively iterating
; until all operators and expressions have been evaluated
(define M_state_side-effect
  (lambda (stmt s goto)
    (cond
      ; Ensure that the statement is an expression that can be evaluated, ie returns the same state is the input is not an expression
      ((not (exp? stmt)) s)
      
      ; Check if the statement creates a new variable
      ((eq? (operator stmt) 'var) (M_state_declare (var-name stmt) (M_value (assignment stmt) s goto) (M_state_side-effect (assignment stmt) s goto) goto))

      ; Check if the statement reassigns a value 
      ((eq? (operator stmt) '=) (M_state_assign (var-name stmt) (M_value (assignment stmt) s goto) (M_state_side-effect (assignment stmt) s goto) goto))
  
      ; Check if the statement is another kind of expression
      ((single_value? stmt) (M_state_side-effect (operand1 stmt) s goto))
      ((dual_value? (operator stmt)) (M_state_side-effect (operand2 stmt) (M_state_side-effect (operand1 stmt) s goto) goto))
      (else s))))

; M_state_while-start
; A helper function for M_state_while using the current continuation to keep track of the state
(define M_state_while-start
  (lambda (condition body-statement s goto)
    (call/cc
     (lambda (break)
       (M_state_while condition body-statement s (goto-setup 'break break goto))))))

; M_state_while
; Given a condition, body, and state, recursively update the state until the condition is met
(define M_state_while
  (lambda (condition body-statement s goto)
    (if (M_boolean condition s goto)
        (M_state_while condition body-statement (M_state-continue body-statement s goto) goto)
        s)))

; M_state_continue
; Given a statement, state, and a point to jump to within a given loop of iteration, return to the top of the loop
(define M_state-continue
  (lambda (stmt s goto)
    (call/cc
     (lambda (continue)
       (M_state stmt s (goto-setup 'continue continue goto))))))

; M_state_try
; Given a block of code that contains a portion of a try/catch block, a state, and a point to go to,
; go to the correct portion of the code based on the portion of the try/catch block
(define M_state_try
  (lambda (stmt s goto)
    (cond
      ((and (catch? stmt) (finally? stmt)) (M_state_block (finally-body stmt) (M_state_try_with-catch stmt s goto) goto))
      ((catch? stmt) (M_state_try_with-catch stmt s goto))
      ((finally? stmt) (M_state_block (finally-body stmt) (M_state_try_without-catch (try-body stmt) s goto) goto))
      (else (M_state_try_without-catch (try-body stmt) s goto)))))

; M_state_try_without-catch
; Given a statement, state, and branching point, attempt to execute the statement as a block of code and if an error occurs, jump to the goto point
(define M_state_try_without-catch
  (lambda (stmt s goto)
    (call/cc
     (lambda (throw)
       (M_state_block stmt s (goto-setup 'throw (lambda (t) (throw (throw-state t))) goto))))))

; M_state_try_with-catch
; Given a statement, state, and branching point, attempt to execute the statement as a block of code but if an error occurs, jump to the associated catch statement
(define M_state_try_with-catch
  (lambda (stmt s goto)
    (call/cc
     (lambda (throw)
       (M_state_block (try-body stmt) s (goto-setup 'throw (lambda (t) (throw (M_state_catch stmt (throw-value t) (remove_layer (throw-state t)) goto))) goto))))))

; M_state_catch
; Execute a block of code that is associated with a try block and handle any errors that occurred during execution of the try block
(define M_state_catch
  (lambda (stmt e s goto)
    (M_state_block (catch-body stmt) (M_state_declare (catch-var stmt) e s goto) goto)))


; M_boolean
; Given a boolean statement and a state, return the boolean value of the statement
(define M_boolean
  (lambda (b-stmt s goto)
    (cond
      ((exp? b-stmt) (M_evaluate b-stmt s goto))
      ((boolean? b-stmt) b-stmt)
      ((boolvalue? b-stmt) (boolvalue b-stmt))
      (else (lookup b-stmt s)))))

; M_value
; Given a statement and a state, retrieve the value returned by the statement
(define M_value
  (lambda (stmt s goto)
    (cond
      ((exp? stmt) (M_evaluate stmt s goto))
      ((null_value? stmt) (nullvalue))
      ((or (boolean? stmt) (number? stmt)) stmt)
      ((boolvalue? stmt) (boolvalue stmt))
      (else (lookup stmt s)))))

; M_value_function
; Given a function name, a list of argument values, a state, and a goto continuation,
; call the given function using the closure provided in the environment and return the state
(define M_value_function
  (lambda (fname args s goto)
    (call/cc
     (lambda (return)
       (cond
         ((not (exist? fname s)) (error "Function does not exist"))
         (else (M_state_list (closure-body (lookup fname s)) (fsetup (closure-params (lookup fname s)) args (add_layer (remove-layer fname s))) (goto-setup 'return return (func-goto goto)))))))))

; M_value_list
; Given a list of values, a state, and a goto continuation,
; return a new list of values that contains the evaluated value of each term, accounting for function calls
(define M_value_list
  (lambda (value-list s goto)
    (value_list-cps value-list s goto (lambda (v) v))))

; value_list-cps
; A continuation passing style helper for M_value_list
(define value_list-cps
  (lambda (value-list s goto return)
    (cond
      ((null? value-list) (return value-list))
      (else (value_list-cps (cdr value-list) s goto (lambda (v) (return (cons (M_value (car value-list) (M_state_side-effect (car value-list) s goto) goto) v))))))))

; M_evaluate
; Given an expression and a state, perform the necessary operations given by the expression and return the new state
(define M_evaluate
  (lambda (exp s goto)
    (cond
      ; Check if a function is being called
      ((eq? (operator exp) 'funcall) (M_value_function (function-name exp) (M_value_list (function-arguments exp) s goto) s goto))
      
      ((unary-? exp) (- 0 (M_value (operand1 exp) s goto)))
      ((eq? (operator exp) '!) (not (M_boolean (operand1 exp) s goto)))
      ((eq? (operator exp) '=) (M_value (assignment exp) (M_state_side-effect (assignment exp) s goto) goto))
      ((value_op? (operator exp)) (operation (operator exp) (M_value (operand1 exp) s goto) (M_value (operand2 exp) (M_state_side-effect (operand1 exp) s goto) goto)))
      ((bool_op? (operator exp)) (operation (operator exp) (M_boolean (operand1 exp) s goto) (M_boolean (operand2 exp) (M_state_side-effect (operand1 exp) s goto) goto)))
      
      (else (error "Expression id not valid")))))

