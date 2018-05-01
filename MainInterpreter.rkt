; ==========================================
; Interpreter, Part 4
; EECS 345 - Programming Language Concepts
;
; Group 12
; Jaafar Bennani
; Alex Hemm
; Kyle Thompson
; ==========================================

(require "Interpreter.rkt")
(require "classParser.scm")
(require "Abstractions.scm")
(require "Readers.scm")

; interpret
; Given a filename of Java/C-like code and a target class, use simpleParser to parse the file and then get the value that block of code returns
; when using the "main" method of the given class
(define interpret
  (lambda (filename classname)
    ; The initial state is empty
    (M_api_main (string->symbol classname) (M_api_list (parser filename) (initapi)))))

(define M_api_main
  (lambda (class api)
    (M_state_main class (csetup class (initstate) api) initgoto api)))
;    (csetup class (initstate) api)))

(define M_api_list
  (lambda (class-list api)
    (cond
      ((null? class-list) api)
      (else (M_api_list (next class-list) (M_api (first class-list) api))))))

(define M_api
  (lambda (pclass api)
    (cond
      ((eq? (operator pclass) 'class)
       (M_api_state_list-cps
        (class-body pclass)
        null
        (blank-state)
        (lambda (v w) (insert-class (class-name pclass) (class-parent pclass) v w api)))))))

(define M_api_state_list-cps
  (lambda (stmt-lis field-lis methodstate return)
   (cond
     ((null? stmt-lis) (return field-lis methodstate))
     (else (M_api_state-cps (first stmt-lis) field-lis methodstate (lambda (v w) (M_api_state_list-cps (cdr stmt-lis) v w return)))))))
                                                 
(define M_api_state-cps
  (lambda (stmt field-lis methodstate return)
    (cond
      ; Check if the statement creates a new variable
      ((eq? (operator stmt) 'var) (return (add-to-field-lis (var-name stmt) field-lis) methodstate))
      
      ; Check if the statement is a function declaration
      ((or (eq? (operator stmt) 'function) (eq? (operator stmt) 'static-function))
       (return field-lis (insert-method (var-name stmt) (fclosure (function-parameters stmt) (function-body stmt)) methodstate))))))


