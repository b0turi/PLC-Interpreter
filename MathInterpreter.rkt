(require "simpleParser.scm")

; Main interpreter function

(define interpret
  (lambda (parser "testfile.txt")
    ))

; M-state operations

(define M_state_list
  (lambda (stmt-lis s)
    (cond
      ((null? statement-list) s)
      (else (M_state_list (cdr stmt-lis) (M_state (car stmt_lis s)))))))

(define M_state
  (lambda (stmt s)
    (cond
      ((null? stmt) s)
      ((eq? (
      


; M-value operations


; Mathematical operations


; M-boolean operations


;=========================================
; Abstractions
;=========================================

; Checks if it is
(define is_operator
  (lambda (math_expr)
    (cond
      ((null? operator) (error 'nothing found'))
      ((eq? '+ (operator) #t))
      ((eq? '- (operator) #t))
      ((eq? '* (operator) #t))
      ((eq? '/ (operator) #t))
      ((eq? '% (operator) #t)))))

(define is_comparison
  (lambda (math_expr)
    (cond
      ((null? operator) (error 'nothing found'))
      ((eq? 

(define operator
  (lambda (math_expr)
    (car math_expr)))

(define operand1
  (lambda (math_expr)
    (cadr math_expr)))

(define operand2
  (lambda (math_expr)
    (caddr math_expr)))
      