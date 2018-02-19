(require "simpleParser.scm")

; Main interpreter function

(define interpret
  (lambda (parser "testfile.txt")))

; M-state operations

(define M_state_list
  (lambda (stmt-lis s)
    (cond
      ((null? statement-list) s)
      (else (M_state_list (cdr stmt-lis) (M_state (car stmt_lis s)))))))

(define M_state
  (lambda (stmt s)
    (cond
      ((null? stmt) s))))
      


; M-value operations


; Mathematical operations

(define evaluate
  (lambda (stmt)
    (cond
     ((null? stmt) (error 'no statement found))
     ((math_operator? (operator stmt)) (math_eval (operator stmt) (operand1 stmt) (operand2 stmt)))
     )))

(define math_eval
  (lambda (oper op1 op2)
    (oper op1 op2)))

(define comp_eval
  (lambda (oper op1 op2)
    
     
     


; M-boolean operations


;=========================================
; Abstractions
;=========================================

; Checks if
(define math_operator?
  (lambda (op)
    (if
      ((null? op) (error 'nothing found))
      (or
       (eq? '+ (op))
       (eq? '- (op))
       (eq? '* (op))
       (eq? '/ (op))
       (eq? '% (op))))))

(define comp_operator?
  (lambda (op)
    (if
      (null? op) (error 'nothing found)
      (or
        (eq? '== (op))
        (eq? '!= (op))
        (eq? '< (op))
        (eq? '> (op))
        (eq? '<= (op))
        (eq? '>= (op))))))

(define bool_operator?
  (lambda (op)
    (if
     (null? op) (error 'nothing found)
     (or
      (eq? '&& (op))
      (eq? '|| (op))
      (eq? '! (op))))))

(define operator
  (lambda (math_stmt)
    (car math_stmt)))

(define operand1
  (lambda (math_stmt)
    (cadr math_stmt)))

(define operand2
  (lambda (math_stmt)
    (caddr math_stmt)))
      