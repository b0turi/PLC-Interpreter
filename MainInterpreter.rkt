; ==========================================
; Interpreter, Part 4
; EECS 345 - Programming Language Concepts
;
; Group 23
; Jaafar Bennani
; Alex Hemm
; Kyle Thompson
; ==========================================

(require "Interpreter.rkt")

; interpret
; Given a filename of Java/C-like code and a target class, use simpleParser to parse the file and then get the value that block of code returns
; when using the "main" method of the given class
(define interpret
  (lambda (filename classname)
    ; The initial state is empty
    (M_state_list_init classname (parser filename) (initstate) initgoto)))

; M_state_list_init
; Top level interpreter code to store all classes in the state, then begin interpreting the class that was passed in as the main
(define M_state_list_init
  (lambda (class stmt-lis s goto)
    (cond
      ((null? stmt-lis) (M_state_main class s goto))
      (else (M_state_list_init class (next stmt-lis) (M_state (class-name (first stmt-lis)) (first stmt-lis) s goto) goto)))))
