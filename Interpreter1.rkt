;Interpreter1 below:
;Vincent Portell, Michael Smith, ____

(require "simpleParser.scm")

(define interpret
  (lambda (filename)
    (parser filename)))

;Example list of statements:
;((var x) (= x 10) (var y (+ (* 3 x) 5)) (while (!= (% y x) 3) (= y (+ y 1)))
;(if (> x y) (return x) (if (> (* x x) y) (return (* x x))
;(if (> (* x (+ x x)) y) (return (* x (+ x x))) (return (- y 1))))))

;I think we should do state in the form: ((x y ...) (5 12 ...)) so its easier
;going forward. 



;go through the list of statements returned by interpret
(define M_statement_list
  (lambda (lis s)
    (cond
      ((null? lis) s)
      ((and (eq? 'var (car (car lis))) (null? (cdr (cdr (car lis))))) (cons (M_state_decl1 (car lis)) (M_statement_list (cdr lis))))
      ;more sutff here

    
   ; (if (null? slist)
   ;     s
   ;     (cons (M_statement_list (cdr slist) s) (M_statement_list (car slist) s)))))

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

(define M_state_decl
  (lambda (var variable)
    (cond 