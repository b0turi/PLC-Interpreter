
; ==========================================
; Interpreter, Part 4
; EECS 345 - Programming Language Concepts
;
; Group 23
; Jaafar Bennani
; Alex Hemm
; Kyle Thompson
; ==========================================

(require "classParser.scm")
(require "Abstractions.scm")

; interpret
; Given a filename of Java/C-like code and a target class, use simpleParser to parse the file and then get the value that block of code returns
; when using the "main" method of the given class
(define interpret
  (lambda (filename classname)
    ; The initial state is empty
    (M_state_list_init classname (parser filename) (initstate) initgoto)))

; M_state_assign
; Given a variable name, value, and a state, update the state so
; that the value of the variable of the given name is the given value
(define M_state_assign
  (lambda (class varname value s goto)
    (replace-value varname value s)))

; M_state_block
; Given a list of statements contained within curly braces {} in the original code, iterate through the block, executing each line,
; and storing a goto value to keep track of where the line of execution should go after the block is completed
(define M_state_block
  (lambda (class stmt-lis s goto)
    ; Each block of code exists within its own layer on top of the state, which must be removed after the block is completed
    (remove_layer (M_state_list class stmt-lis (add_layer s) (block_goto goto)))))

; M_state_declare
; takes in a varriable, a value, and a state , checks if the varriable has already beed declared, and adds the varriable to the state with the value given
(define M_state_declare
  (lambda (class varname value s)
    (cond
      ((exist-top? varname s) (error "Redefining"))
      (else (class insert-var varname value s)))))


; M_state_function
; Given a function name, a list of argument values, a state, and a goto continuation,
; return the function environment after the function has been executed
(define M_state_function
  (lambda (class fname args s goto)
    (call/cc
     (lambda (return)
       (cond
         ((not (exist? fname s)) (error "Function does not exist"))
         (else (begin (M_state_list class (fclosure-body (lookup fname s)) (remove-layer fname (fsetup (fclosure-params (lookup fname s)) args (add_layer s) goto)) (goto-setup 'return (gotoreturn return s) (func-goto s goto))) s)))))))

; M_state_if
; Given a condition, its relevant then and else statements, and a state, return the 
; new state with the relevant statement evaluated, based on the condition
(define M_state_if
  (lambda (condition then-statement else-statement s goto)
    (if (M_boolean class condition s goto)
        (M_state class then-statement s goto)
        (M_state class else-statement s goto))))

; M_state_list_init
; Top level interpreter code to store all classes in the state, then begin interpreting the class that was passed in as the main
(define M_state_list_init
  (lambda (class stmt-lis s goto)
    (cond
      ((null? stmt-lis) (M_state_main class s goto))
      (else (M_state_list (next stmt-lis) (M_state (class-name (first stmt-lis)) (first stmt-lis) s goto) goto)))))

; M_state_list
; Given a list of statements and a state, evaluate each line with M_state and return the state
; after each line has been evaluated
(define M_state_list
  (lambda (class stmt-lis s goto)
    (cond
      ((null? stmt-lis) s)
      (else (M_state_list class (next stmt-lis) (M_state class (first stmt-lis) s goto) goto)))))

; M_state_main
; Given an environment and a goto continuation,
; search for the main function and call it using M_value_function, passing the goto continuation as well
(define M_state_main
  (lambda (class s goto)
    (M_value_function class 'main '() s goto)))
    
; M_state
; Given a statement and a state, evaluate the statement and return the new state
(define M_state
  (lambda (class stmt s goto)
    (cond
      ; Ensure that the statement is an expression that can be evaluated, ie returns the same state is the input is not an expression
      ((not (exp? stmt)) s)
      
      ; Check if the statement creates a new variable
      ((eq? (operator stmt) 'var) (M_state_declare class (var-name stmt) (M_value (assignment stmt) s goto) (M_state_side-effect (assignment stmt) s goto)))
      
      ; Check if the statement is a function declaration
      ((eq? (operator stmt) 'function) (M_state_declare class (function-name stmt) (fclosure (function-parameters stmt) (function-body stmt)) s))

      ; Check if the statement is a branching statement with a goto
      ((eq? (operator stmt) 'return) (goto 'return (realvalue (M_value class (return-exp stmt) (M_state_side-effect (assignment stmt) s goto) goto))))
      ((eq? (operator stmt) 'throw) (goto 'throw (throws (realvalue (M_value class (return-exp stmt) s goto)) s)))
      ((state-goto? (operator stmt)) (goto (operator stmt) s))
      ((eq? (operator stmt) 'try) (M_state_try class stmt s goto))
      
      ; Check if the statement is a branching statement with a condition
      ((eq? (operator stmt) 'while) (M_state_while-start class (condition stmt) (body stmt) s goto))
      ((eq? (operator stmt) 'if) (M_state_if class (condition stmt) (then stmt) (else stmt) s goto))

      ; Check if the statement is a block of code, defined by "begin" in the parser
      ((eq? (operator stmt) 'begin) (M_state_block class (block-body stmt) s goto))
      
      ; Check if a function is being called
      ((eq? (operator stmt) 'funcall) (M_state_function class (function-name stmt) (function-arguments stmt) s goto))

      ; Check if the statement is a class declaration
      ((eq? (operator stmt) 'class) (M_state_declare (nullvalue) (class-name stmt) (cclosure (class-parent stmt) (class-fields (class-body stmt)) (class-functions (class-body stmt)) (M_state_class_function_closures (class-body stmt) s)) s))
      
      (else (M_state_side-effect class stmt s goto)))))

; M_state_side_effect
; Given a statement, a state, and a goto continuation,
; Evaluate the statement but also account for any side effects that may occur by recursively iterating
; until all operators and expressions have been evaluated
(define M_state_side-effect
  (lambda (class stmt s goto)
    (cond
      ; Ensure that the statement is an expression that can be evaluated, ie returns the same state is the input is not an expression
      ((not (exp? stmt)) s)
      
      ; Check if the statement reassigns a value 
      ((eq? (operator stmt) '=) (M_state_assign class (var-name stmt) (M_value class (assignment stmt) s goto) (M_state_side-effect class (assignment stmt) s goto) goto))
  
      ; Check if the statement is another kind of expression
      ((single_value? stmt) (M_state_side-effect class (operand1 stmt) s goto))
      ((dual_value? (operator stmt)) (M_state_side-effect class (operand2 stmt) (M_state_side-effect class (operand1 stmt) s goto) goto))
      (else s))))

; M_state_class_function_closures
(define M_state_class_function_closures
  (lambda (class body s goto)
    (cond
      ((null? body) s)
      ((or (eq? (operator (first body)) 'function) (eq? (operator (first body)) 'static-function)) (M_state_class_function_closures class (next body) (M_state (first body) s goto) goto))
      (else (M_state_class_function_closures class (next body) s goto)))))

; M_state_while-start
; A helper function for M_state_while using the current continuation to keep track of the state
(define M_state_while-start
  (lambda (class condition body-statement s goto)
    (call/cc
     (lambda (break)
       (M_state_while class condition body-statement s (goto-setup 'break break goto))))))

; M_state_while
; Given a condition, body, and state, recursively update the state until the condition is met
(define M_state_while
  (lambda (class condition body-statement s goto)
    (if (M_boolean class condition s goto)
        (M_state_while class condition body-statement (M_state-continue class body-statement s goto) goto)
        s)))

; M_state_continue
; Given a statement, state, and a point to jump to within a given loop of iteration, return to the top of the loop
(define M_state-continue
  (lambda (class stmt s goto)
    (call/cc
     (lambda (continue)
       (M_state class stmt s (goto-setup 'continue continue goto))))))

; M_state_try
; Given a block of code that contains a portion of a try/catch block, a state, and a point to go to,
; go to the correct portion of the code based on the portion of the try/catch block
(define M_state_try
  (lambda (class stmt s goto)
    (cond
      ((and (catch? stmt) (finally? stmt)) (M_state_block class (finally-body stmt) (M_state_try_with-catch class stmt s goto) goto))
      ((catch? stmt) (M_state_try_with-catch class stmt s goto))
      ((finally? stmt) (M_state_block class (finally-body stmt) (M_state_try_without-catch class (try-body stmt) s goto) goto))
      (else (M_state_try_without-catch class (try-body stmt) s goto)))))

; M_state_try_without-catch
; Given a statement, state, and branching point, attempt to execute the statement as a block of code and if an error occurs, jump to the goto point
(define M_state_try_without-catch
  (lambda (class stmt s goto)
    (call/cc
     (lambda (throw)
       (M_state_block class stmt s (goto-setup 'throw (lambda (t) (throw (throw-state t))) goto))))))

; M_state_try_with-catch
; Given a statement, state, and branching point, attempt to execute the statement as a block of code but if an error occurs, jump to the associated catch statement
(define M_state_try_with-catch
  (lambda (class stmt s goto)
    (call/cc
     (lambda (throw)
       (M_state_block class (try-body stmt) s (goto-setup 'throw (lambda (t) (throw (M_state_catch stmt class (throw-value t) (remove_layer (throw-state t)) goto))) goto))))))

; M_state_catch
; Execute a block of code that is associated with a try block and handle any errors that occurred during execution of the try block
(define M_state_catch
  (lambda (class stmt e s goto)
    (M_state_block class (catch-body stmt) (M_state_declare class (catch-var stmt) e s) goto)))


; M_boolean
; Given a boolean statement and a state, return the boolean value of the statement
(define M_boolean
  (lambda (class b-stmt s goto)
    (cond
      ((exp? b-stmt) (M_evaluate class b-stmt s goto))
      ((boolean? b-stmt) b-stmt)
      ((boolvalue? b-stmt) (boolvalue b-stmt))
      (else (lookup b-stmt s)))))

; M_value
; Given a statement and a state, retrieve the value returned by the statement
(define M_value
  (lambda (class stmt s goto)
    (cond
      ((exp? stmt) (M_evaluate class stmt s goto))
      ((null_value? stmt) (nullvalue))
      ((or (boolean? stmt) (number? stmt)) stmt)
      ((boolvalue? stmt) (boolvalue stmt))
      (else (lookup stmt s)))))

; M_value_function
; Given a containing class, function name, a list of argument values, a state, and a goto continuation,
; call the given function using the function closure provided in the environment and return the state
(define M_value_function
  (lambda (class fname args s goto)
    (call/cc
     (lambda (return)
       (cond
         ((not (exist? class fname s)) (error "Function does not exist"))
         (else (M_state_list class (fclosure-body (lookup fname s)) (remove-layer fname (fsetup (fclosure-params (lookup fname s)) args (add_layer s) goto)) (goto-setup 'return return (func-goto s goto)))))))))

; M_evaluate
; Given an expression and a state, perform the necessary operations given by the expression and return the new state
(define M_evaluate
  (lambda (class exp s goto)
    (cond
      ; Check if a function is being called
      ((eq? (operator exp) 'funcall) (M_value_function class (function-name exp) (function-arguments exp) s goto))
      
      ((unary-? exp) (- 0 (M_value class (operand1 exp) s goto)))
      ((eq? (operator exp) '!) (not (M_boolean class (operand1 exp) s goto)))
      ((eq? (operator exp) '=) (M_value class (assignment exp) (M_state_side-effect class (assignment exp) s goto) goto))
      ((value_op? (operator exp)) (operation (operator exp) (M_value class (operand1 exp) s goto) (M_value class (operand2 exp) (M_state_side-effect class (operand1 exp) s goto) goto)))
      ((bool_op? (operator exp)) (operation (operator exp) (M_boolean class (operand1 exp) s goto) (M_boolean class (operand2 exp) (M_state_side-effect class (operand1 exp) s goto) goto)))
      
      (else (error "Expression id not valid")))))

; fsetup
; Given a list of parameters, arguments, and a state, add each of the parameters into the state
; populated with the argument values, and return the new state
(define fsetup
  (lambda (params args s goto)
    (cond
      ((> (fparam-length params) (length args)) (error "Too few arguments"))
      ((< (fparam-length params) (length args)) (error "Too many arguments"))
      (else (fsetup-cps params args s goto (lambda (v) v))))))

; fsetup-cps
; A continuation passing style helper for fsetup
(define fsetup-cps
  (lambda (params args s goto return)
    (cond
      ((null? params) (return s))
      ((eq? '& (car params)) (fsetup-cps (cddr params) (cdr args) (M_state_side-effect class (car args) s goto) goto (lambda (v) (return (rinsert-var (cadr params) (rlookup (car args) s) v)))))
      (else (fsetup-cps (cdr params) (cdr args) (M_state_side-effect class (car args) s goto) goto (lambda (v) (return (insert-var (car params) (M_value class (car args) s goto) v))))))))