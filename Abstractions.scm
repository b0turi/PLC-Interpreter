
; ==========================================
; Abstractions, Part 3
; EECS 345 - Programming Language Concepts
;
; Group 23
; Jaafar Bennani
; Alex Hemm
; Kyle Thompson
; ==========================================

#lang racket
(provide (all-defined-out))

; ===== Function "readers" =====

; function-arguments
; Given a statement that is assumed to be a function call, retrieve the list of arguments to call the function with
(define function-arguments
  (lambda (stmt)
    (cddr stmt)))

; function-body
; Given a statement that is assumed to be a function declaration, retrieve the body of the function
(define function-body
  (lambda (stmt)
    (cadddr stmt)))

; function-name
; Given a statement that is assumed to be a function declaration, retrieve the name of the function
(define function-name
  (lambda (stmt)
    (cadr stmt)))

; function-parameters
; Given a statement that is assumed to be a function declaration, retrieve the list of parameters for the function
(define function-parameters
  (lambda (stmt)
    (caddr stmt)))

; return-exp
; Given a statement that is known to be a return statement,
; return the value the statement dictates is to be returned
(define return-exp
  (lambda (stmt)
    (cadr stmt)))

; operator
; Given a statement, retrieve the operator in the statement
(define operator
  (lambda (stmt)
    (cond
      ((null? stmt) (error 'thereIsNoStatement))
      ((list? stmt) (car stmt))
      (else (error 'invalidStatement)))))

; operand1
; Given a statement, retrieve the first operand
(define operand1
  (lambda (math_stmt)
    (cadr math_stmt)))

; operand2
; Given a statement that is known to be a binary expression,
; retrieve the second operand
(define operand2
  (lambda (math_stmt)
    (caddr math_stmt)))

; try-body
; Given a statement known to be a try/catch block, retrieve the body of the try block
(define try-body
  (lambda (stmt)
    (cadr stmt)))

; catch?
; Returns whether a given statement is a catch block
(define catch?
  (lambda (stmt)
    (cond
      ((null? (caddr stmt)) #f)
      ((eq? (operator (caddr stmt)) 'catch) #t)
      (else #f))))

; catch-var
; As a part of error handling, grab the value to be returned within a catch block from a complete try/catch statement
(define catch-var
  (lambda (stmt)
    (caadr (caddr stmt))))

; catch-body
; Given a statement known to be a try/catch block, retrieve the body of the catch block
(define catch-body
  (lambda (stmt)
    (caddr (caddr stmt))))

; finally?
; Returns whether a given statement is a finally block
(define finally?
  (lambda (stmt)
    (cond
      ((null? (cadddr stmt)) #f)
      ((eq? (operator (cadddr stmt)) 'finally) #t)
      (else #f))))
; finally-body
; Given a statement that is known to be a try/catch/finally block, retrieve the body of the associated finally block
(define finally-body
  (lambda (stmt)
    (cadr (cadddr stmt))))

; Function closures

; fclosure
; Given a list of parameters and the body of a function, return the two listed together
; To be used as the basis for a closure function
(define fclosure
  (lambda (params body)
    (list params body)))

; fclosure-body
; Given a statement that is assumed to be a closure (as created by the helper function "fclosure"), retrieve the body of the function
(define fclosure-body
  (lambda (stmt)
    (cadr stmt)))

; fclosure-params
; Given a statement that is assumed to be a closure (as created by the helper function "fclosure"), retrieve the parameters of the function
(define fclosure-params
  (lambda (stmt)
    (car stmt)))

; Class Closure

;cclosure
(define cclosure
  (lambda (parent ifields fnames fclosures)
    (list parent ifields fnames fclosures)))

(define cclosure-parent
  (lambda (stmt)
    (car stmt)))

(define cclosure-ifields
  (lambda (stmt)
    (cadr stmt)))

(define cclosure-fnames
  (lambda (stmt)
    (caddr stmt)))

(define cclosure-fclosures
  (lambda (stmt)
    (cadddr stmt)))

; ==== If and While Statement Helpers ====

; condition
; Given a statement that is known to be either an if statement or a while statement,
; retrieve the condition that will determine which substatement runs in an if statement,
; or whether the body will be executed again in a while statement
(define condition
  (lambda (stmt)
    (cadr stmt)))

; then
; Given a statement that is known to be an if statement,
; retrieve the substatement to run when the condition is true
(define then
  (lambda (stmt)
    (caddr stmt)))

; else
; Given a statement that is known to be an if statement, retrieve the substatement
; to run when the condition is false, or '() if there is no else
(define else
  (lambda (stmt)
    (if (null? (cdddr stmt))
        '()
        (cadddr stmt))))

; body
; Given a statement that is known to be a while statement, retrieve the body
(define body
  (lambda (stmt)
    (caddr stmt)))

; block-body
; Given a statment that is known to be a block of code, retrieve the body of the block
(define block-body
  (lambda (stmt)
    (cdr stmt)))

; ===== STATE =====

; insert-var
; Given a variable name, value, and a state, insert the variable with the given value into the state,
; accounting for boolean literals, and return the new state with the variable added
(define insert-var
  (lambda (varname value s)
    (cons (list (cons varname (caar s)) (cons (box value) (cadar s))) (cdr s))))

; rinsert-var
; Given a variable name, value, and a state, insert the variable with the given value into the state,
; accounting for boolean literals, and return the new state with the variable added
(define rinsert-var
  (lambda (varname box s)
    (cons (list (cons varname (caar s)) (cons box (cadar s))) (cdr s))))

; replace-value
; Given a variable name, value, and state, find the location within the state where the given variable name is stored and replace its value, and return the new state
(define replace-value
  (lambda (varname value s)
    (if (null? s)
        (error "using before declaring")
        (replaceval-cps varname value (caar s) (cadar s) (lambda (v1 v2) (cons (list v1 v2) (cdr s))) (lambda () (cons (car s) (replace-value varname value (cdr s))))))))

; tail recursive helper for replace-value
(define replaceval-cps
  (lambda (varname value namelis valuelis return break)
    (cond
      ((null? namelis) (break))
      ((equal? varname (car namelis)) (begin (set-box! (car valuelis) value) (return namelis valuelis)))
      (else (replaceval-cps varname value (cdr namelis) (cdr valuelis) (lambda (l1 l2) (return (cons (car namelis) l1) (cons (car valuelis) l2))) break)))))

; add_layer
; Add a new layer of variables to the top of the list of layers within the state to handle scope of specific variables
(define add_layer
  (lambda (s)
    (cons '(() ()) s)))

; remove_layer
; Remove the top layer of the list of layers within the state, and return the new state
(define remove_layer
  (lambda (s)
    (if (null? s)
        (error "No layers present")
        (cdr s))))

; remove_layer
; An alternate version of remove_layer that removes a specific state for a function
(define remove-layer
  (lambda (fname s)
    (cons (car s) (remove-layer-helper fname (cdr s)))))

(define remove-layer-helper
  (lambda (fname s)
    (cond
      ((exist-top? fname s) s)
      (else (remove-layer-helper fname (cdr s))))))

; ===== Lookup =====
; lookup
; Given a variable name and a state, check if that variable is defined in that state.
; Throws and appropriate error if the variable doesn't exist in the state or is uninitialized
(define lookup
  (lambda (varname s)
    (if (null? s)
        (error "using before declaring")
        (lookup-cps varname (caar s) (cadar s) (lambda (v) v) (lambda () (lookup varname (cdr s)))))))
  
; lookup-cps
; A tail recursive helper for lookup
(define lookup-cps
  (lambda (name namelis valuelis return break)
    (cond
      ((null? namelis) (break))
      ((and (eq? name (car namelis)) (null_value? (car valuelis))) (error "using before assigning"))
      ((eq? name (car namelis)) (return (unbox (car valuelis))))
      (else (lookup-cps name (cdr namelis) (cdr valuelis) return break)))))

; rlookup
; Given a variable name and a state, check if that variable is defined in that state.
; Throws and appropriate error if the variable doesn't exist in the state or is uninitialized
(define rlookup
  (lambda (varname s)
    (if (null? s)
        (error "using before declaring")
        (rlookup-cps varname (caar s) (cadar s) (lambda (v) v) (lambda () (rlookup varname (cdr s)))))))
  
; rlookup-cps
; A tail recursive helper for lookup
(define rlookup-cps
  (lambda (name namelis valuelis return break)
    (cond
      ((null? namelis) (break))
      ((and (eq? name (car namelis)) (null_value? (car valuelis))) (error "using before assigning"))
      ((eq? name (car namelis)) (return (car valuelis)))
      (else (rlookup-cps name (cdr namelis) (cdr valuelis) return break)))))

; boolvalue
; Return the literal boolean value of a given value that is assumed to be either the truevalue or falsevalue atom
(define boolvalue
  (lambda (v)
    (cond
      ((eq? v (truevalue)) #t)
      ((eq? v (falsevalue)) #f)
      (else (error "invalid boolean")))))

; boolvalue?
; Return whether a given value is either the truevalue or falsevalue atom
(define boolvalue?
  (lambda (v)
    (or (eq? v (truevalue)) (eq? v (falsevalue)))))

; ===== Exist? =====
; exist?
; Given a name and a state, check if the given name is defined anywhere in any of the layers of the state
(define exist?
  (lambda (name s)
    (if (null? s)
        #f
        (exist?-cps name (caar s) (lambda (v) v) (lambda () (exist? name (cdr s)))))))

; exist?-cps
; A tail recursive helper for exist?
(define exist?-cps
  (lambda (name namelis return break)
    (cond
      ((null? namelis) (break))
      ((eq? name (car namelis)) (return #t))
      (else (exist?-cps name (cdr namelis) return break)))))

; exist-top?
; Given a name and a state, check if the given name is defined within the top layer of the state
(define exist-top?
  (lambda (name s)
    (exist-top?-cps name (caar s) (lambda (v) v))))

; exist-top?-cps
; A tail recursive helper for exist-top?
(define exist-top?-cps
  (lambda (name namelis return)
    (cond
      ((null? namelis) (return #f))
      ((eq? name (car namelis)) (return #t))
      (else (exist-top?-cps name (cdr namelis) return)))))

; ===== Miscellaneous =====

; next
; Given a list, return the list without its first element,
; so the first element is the "next" element of the original list
(define next
  (lambda (lis)
    (cdr lis)))

; var-name
; Given a statement known to be the declaration of a variable,
; retrieve the name of the new variable
(define var-name
  (lambda (stmt)
    (cadr stmt)))

; assignment
; Given a statement known to be an assignment to a variable,
; retrieve the value that is to be assigned to the variable
(define assignment
  (lambda (stmt)
    (if (null? (cddr stmt))
        'null
        (caddr stmt))))

; realvalue
; Given a value, check if the value is a boolean literal, and return
; the equivalent atom for printing, or return the original value if not
(define realvalue
  (lambda (v)
    (cond
      ((eq? v #t) (truevalue))
      ((eq? v #f) (falsevalue))
      (else v))))

; ===== BRANCHING =====

; goto-setup
; Given a name, a function to execute, and a branching function, return a function that can be used to check
; other functions against a stored function and either execute that function or a branching function if the given
; value is not the same
(define goto-setup
  (lambda (name function goto)
    (lambda (f v)
      (if (eq? f name)
          (function v)
          (goto f v)))))

; gotoerror
; A default error function for branching
(define gotoerror2
  (lambda (a b)
    (error "error")))

(define gotoerror1
  (lambda (a b)
    (error "error")))

; initgoto
; Uses goto-setup to define a function that handles errors when a "throw" statement is encountered
(define initgoto
    (goto-setup 'throw (lambda (s) (if (number? (throw-value s)) (error (string-append "error: " (number->string (throw-value s)))) (error (throw-value s)))) gotoerror2))

; block_goto
; Uses goto-setup to define a function that jumps to a given goto spot depending on whether either break or continue are called in the block
(define block_goto
  (lambda (goto)
    (goto-setup 'break (lambda (s) (goto 'break (remove_layer s))) (goto-setup 'continue (lambda (s) (goto 'continue (remove_layer s))) goto))))

; state-goto?
; Given an operation, determine if the state will need to change based on whether the operation is 'break or 'continue
(define state-goto?
  (lambda (op)
    (or
      (eq? op 'break)
      (eq? op 'continue))))

(define func-goto
  (lambda (s goto)
    (goto-setup 'throw (lambda (lis) (goto 'throw (list (throw-value lis) s))) (goto-setup 'break gotoerror1 (goto-setup 'continue gotoerror1 goto)))))

(define gotoreturn
  (lambda (return s)
    (lambda (v) (return s))))

; throws
; Given a value and a state, combine the two in a list for the completion of the goto function defined with goto-setup
(define throws
  (lambda (v s)
    (list v s)))

; throw-value
; Given a statement that is known to be a throws statement, retrieve the value that is to be thrown
(define throw-value
  (lambda (stmt)
    (car stmt)))

; throw-state
; Given a statement that is known to be a throws statement, retrieve the current state when the throws operation is called
(define throw-state
  (lambda (lis)
    (cadr lis)))


; ===== OPERATIONS =====

; unary-?
; Given a statement, return whether the statement is a unary expression with '- as the operator
; as a boolean value
(define unary-?
  (lambda (stmt)
    (and (equal? (car stmt) '-) (null? (cddr stmt)))))

; single_value?
; Given a statement, return a boolean value as to whether the statement has only one operand
(define single_value?
  (lambda (stmt)
    (or (eq? '! (operator stmt)) (unary-? stmt))))

; dual_value?
; Given a statement, return a boolean value as to whether the statement has two operands
(define dual_value?
  (lambda (op)
    (or (value_op? op) (bool_op? op))))

; value_op?
; Given an operator, return a boolean value as to whether the operator is a value operator,
; or, an operator that requires two operands
(define value_op?
  (lambda (op)
    (or
     (eq? '+ op)
     (eq? '- op)
     (eq? '* op)
     (eq? '/ op)
     (eq? '% op)
     (eq? '== op)
     (eq? '!= op)
     (eq? '< op)
     (eq? '> op)
     (eq? '<= op)
     (eq? '>= op))))

; bool_op?
; Given an operator, return a boolean value as to whether the operator is a boolean operator,
; or, an operator that can only operate on two booleans
(define bool_op?
  (lambda (op)
    (or (eq? '&& op) (eq? '|| op))))

; operation
; Given an operator and two operands, return the result of the operation on the two operands
(define operation
  (lambda (op v1 v2)
    (cond
      ; Value operators
      ((eq? '+ op) (+ v1 v2))
      ((eq? '- op) (- v1 v2))
      ((eq? '* op) (* v1 v2))
      ((eq? '/ op) (quotient v1 v2))
      ((eq? '% op) (remainder v1 v2))
      ((eq? '== op) (= v1 v2))
      ((eq? '!= op) (not (= v1 v2)))
      ((eq? '< op) (< v1 v2))
      ((eq? '> op) (> v1 v2))
      ((eq? '<= op) (<= v1 v2))
      ((eq? '>= op) (>= v1 v2))

      ; Boolean operators
      ((eq? '&& op) (and v1 v2))
      ((eq? '|| op) (or v1 v2))
      (else (error "Operator not valid")))))

; null_value?
; Given a value, return a boolean value as to whether the value is the predefined null value
(define null_value?
  (lambda (v)
    (eq? v (nullvalue))))

; exp?
; Given an atom, return a boolean value as to whether the given atom is a pair, or, can be
; interpreted as an expression
(define exp?
  (lambda (v)
    (pair? v)))

; ===== Predefined values =====
(define nullvalue
  (lambda ()
    'null))

(define truevalue
  (lambda ()
    'true))

(define falsevalue
  (lambda ()
    'false))

(define initstate
  (lambda ()
    (add_layer null)))

(define returnvalue (lambda (v) v))


(define fparam-length
  (lambda (lis)
    (fparam-length-acc lis 0)))

(define fparam-length-acc
  (lambda (lis acc)
    (cond
      ((null? lis) acc)
      ((eq? (car lis) '&) (fparam-length-acc (cdr lis) acc))
      (else (fparam-length-acc (cdr lis) (+ 1 acc))))))