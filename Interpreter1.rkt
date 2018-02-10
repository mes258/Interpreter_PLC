;Interpreter1 below:
;Vincent Portell, Michael Smith, ____

(require "simpleParser.scm")

(define interpret
  (lambda (filename)
    (parser filename)))

;go through the list of statements returned by interpret
(define M_statement_list
  (lambda (slist s)
    (if (null? slist)
        s
        (cons (M_statement_list (cdr slist) s) (M_statement_list (car slist) s)))))

(define M_state_if
  (lambda (condition then else s)
    (if (M_bool condition s)
        (M_state then s)
        (M_state else s))))

;need M_bool, M_state, M_value for:
; variable declaration 	(var variable) or (var variable value)
;assignment 	(= variable expression)
;return 	(return expression)
;if statement 	(if conditional then-statement optional-else-statement)
;while statement 	(while conditional body-statement)
