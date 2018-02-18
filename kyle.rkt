(require "simpleParser.scm")
(define interpret
  (lambda (filename)
    (findvars* (parser filename))))

; findvars* - get all declarations of variables from the parse tree and return two lists with each variable's name and initial value
(define findvars*
  (lambda (tree)
    (cond
      ((null? tree) '(() ()))
      
      ((M_declaration (car tree)) (cons (cons (M_name (car tree)) (car (findvars* (cdr tree)))) (cons (cons (M_value (car tree)) (cadr (findvars* (cdr tree)))) '())))
      (else (cons (car (findvars* (cdr tree))) (cons (cadr (findvars* (cdr tree))) '()))))))

; ABSTRACTORS
(define M_declaration
  (lambda (stmt)
    (if (eq? (car stmt) '=)
        #t
        #f)))

; varName assumes that the given statement is an assignment statement, because isAssignment has already been called
(define M_name
  (lambda (stmt)
    (cadr stmt)))

; varValue assumes that the given statement is an assignment statement, because isAssignment has already been called
(define M_value
  (lambda (stmt)
    (caddr stmt)))

