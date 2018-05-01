
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
(require "Readers.scm")

; ===== Class "readers" =====

; class-name
(define class-name
  (lambda (stmt)
    (cadr stmt)))

; class-extends
(define class-extends
  (lambda (stmt)
    (caddr stmt)))

; class-parent
(define class-parent
  (lambda (stmt)
    (cond
      ((null? (class-extends stmt)) (nullvalue))
      (else (cadr (class-extends stmt))))))

; class-body
(define class-body
  (lambda (stmt)
    (cadddr stmt)))

; class-fields
(define class-fields
  (lambda (body)
    (class-fields-cps body (lambda (x y) (list x y)))))

; class-fields-cps
(define class-fields-cps
  (lambda (body return)
    (cond
      ((null? body) (return '() '()))
      ((eq? (operator (car body)) 'var) (class-fields-cps (cdr body) (lambda (x y) (return (cons (var-name (car body)) x) (cons (assignment (car body)) y)))))
      (else (class-fields-cps (cdr body) return)))))

; class-functions
(define class-functions
  (lambda (body)
    (class-functions-cps body (lambda (v) v))))

; class-functions-cps
(define class-functions-cps
  (lambda (body return)
    (cond
      ((null? body) (return '()))
      ((or (eq? (operator (car body)) 'function) (eq? (operator (car body)) 'static-function)) (class-functions-cps (cdr body) (lambda (v) (return (cons (function-name (car body)) v)))))
      (else (class-functions-cps (cdr body) return)))))


; Function closures

; fclosure
; Given a list of parameters and the body of a function, return the two listed together
; To be used as the basis for a closure function
(define fclosure
  (lambda (params body)
    (list (cons 'super (cons 'this params)) body)))

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

; ===== Closure =====

; insert-class
; Given a variable name, value, and a state, insert the variable with the given value into the state,
; accounting for boolean literals, and return the new state with the variable added
(define insert-class
  (lambda (classname parent fields methods api)
    (list (cons classname (name-lis api)) (cons parent (class-parent-lis api)) (cons fields (fieldlis-lis api)) (cons methods (methodstate-lis api)))))

(define initapi (lambda () '(()()()())))

(define insert-method
  (lambda (method-name method-closure s)
    (list (cons method-name (list-ref s 0)) (cons method-closure (list-ref s 1)))))

(define name-lis
  (lambda (s)
    (list-ref s 0)))

(define class-parent-lis
  (lambda (api)
    (list-ref api 1)))

(define bind-lis
  (lambda (s)
    (list-ref s 1)))

(define fieldlis-lis
  (lambda (api)
    (list-ref api 2)))

(define methodstate-lis
  (lambda (api)
    (list-ref api 3)))

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

(define add-to-field-lis
  (lambda (varname lis)
    (cons varname lis)))

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
    (cons blank-state s)))

(define blank-state (lambda () '(() ())))

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
        (vlookups (unbox (vlookup varname (caar s) (reverse (cadar s)))) (error "using before assigning") (lookup varname (cdr s))))))

(define lookup-class
  (lambda (classname s)
    (if (null? s)
        (error "using before declaring")
        (vlookups (vlookup classname (caar s) (reverse (cadar s))) (error "using before assigning") (error "using before declaring")))))

; rlookup
; Given a variable name and a state, check if that variable is defined in that state.
; Throws and appropriate error if the variable doesn't exist in the state or is uninitialized
(define rlookup
  (lambda (varname s)
    (if (null? s)
        (error "using before declaring")
        (vlookups (vlookup varname (caar s) (reverse (cadar s))) (error "using before assigning") (lookup varname (cdr s))))))

(define vlookups
  (lambda (v break continue)
    (cond
      ((null_value? v) (break))
      ((eq? (noValue) v) (continue))
      (else v))))

(define vlookup
  (lambda (name name-lis value-lis)
    (lookup-cps name name-lis value-lis (lambda (v) v) (noValue))))

; lookup-cps
; A tail recursive helper for lookup
(define lookup-cps
  (lambda (name namelis valuelis return break)
    (cond
      ((null? namelis) (break))
      ((eq? name (car namelis)) (return (list-ref valuelis (length (cdr namelis)))))
      (else (lookup-cps name (cdr namelis) valuelis return break)))))

(define noValue 'valueNotFound)

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
  (lambda (class name s)
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


(define fparam-length
  (lambda (lis)
    (- (fparam-length-acc lis 0) 2)))

(define fparam-length-acc
  (lambda (lis acc)
    (cond
      ((null? lis) acc)
      ((eq? (car lis) '&) (fparam-length-acc (cdr lis) acc))
      (else (fparam-length-acc (cdr lis) (+ 1 acc))))))

(define initstate
  (lambda ()
    (add_layer null)))