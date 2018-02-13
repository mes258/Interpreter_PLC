;Interpreter1 below:
;Vincent Portell, Michael Smith, ____

(require "simpleParser.scm")

;(define interpret
 ; (lambda (filename)
 ;   (parser filename)))

;Example list of statements:
;((var x) (= x 10) (var y (+ (* 3 x) 5)) (while (!= (% y x) 3) (= y (+ y 1)))
;(if (> x y) (return x) (if (> (* x x) y) (return (* x x))
;(if (> (* x (+ x x)) y) (return (* x (+ x x))) (return (- y 1))))))

;I think we should do state in the form: ((x y ...) (5 12 ...)) so its easier
;going forward. 
(define p1 (build-path (current-directory) "code.txt"))


;(parser p1)
(define lis (parser p1))
;need to pass lis into M_list
;go through the list of statements returned by interpret
(define M_list
  (lambda (lis s)
    (cond
      ((null? lis) s)
      ((and (eq? (type lis) 'var) (null? (caddr lis))) (cons (M_state_decl1 (fir lis) (sec lis) s) (M_list (cdr lis) s)))
      ((eq? (type lis) 'var) (cons (M_state_decl2 (fir lis) (sec lis) (thr lis) s) (M_list (cdr lis) s)))
      ((eq? (type lis) 'while) (cons (M_state_while (fir lis) (sec lis) (thr lis) (frth lis) s) (M_list (cdr lis) s)))
      ((eq? (type lis) 'return) (cons (M_state_return (fir lis) (sec lis)) (M_list (cdr lis) s)))
      ((and (eq? (type lis) 'if) (null? (cdddar lis))) (cons (M_state_if (fir lis) (sec lis) s) (M_list (cdr lis) s)))
      ((eq? (type lis) 'if) (cons (M_state_if_else (fir lis) (sec lis) (thr lis) s) (M_list (cdr lis) s)))
      (else lis)))) ; need to fix parameters of functions - get each part of (car lis) not just (car lis) for the parameter

;abstraction for M_list
(define type caar);type of line
(define fir caar);first element 
(define sec cadr);second element
(define thr caddr);third element
(define frth cadddr);fourth element


;need M_bool, M_state, M_value for:
;variable declaration 	(var variable) or (var variable value)
;assignment 	(= variable expression)
;return 	(return expression)
;if statement 	(if conditional then-statement optional-else-statement)
;while statement 	(while conditional body-statement)

;M_State_stuff
(define M_state_decl1
  (lambda (var variable s)
    (cond
      )))

(define M_state_decl2
  (lambda (var variable value s)
    (cond
      )))

(define M_state_while
  (lambda (while condit body s) 
    (cond
      )))

(define M_state_return
  (lambda (return exp)
    (cond
      ((null? exp) '())
      (else (M_bool_op exp)))))

(define M_state_if_else
  (lambda (condition then else s)
    (if (M_bool_op condition)
        (M_list then s)
        (M_list else s))))

(define M_state_if
  (lambda (condition then s)
    (if (M_bool_op condition)
        (M_list then s)
        `())))


;M_value_stuff

(define M_value_op
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((not (list? lis)) lis)
      ((eq? (operator lis) '+) (+ (M_value_op (operand1 lis)) (M_value_op (operand2 lis))))
      ((eq? (operator lis) '-) (- (M_value_op (operand1 lis)) (M_value_op (operand2 lis))))
      ((eq? (operator lis) '*) (* (M_value_op (operand1 lis)) (M_value_op (operand2 lis))))
      ((eq? (operator lis) '/) (quotient (M_value_op (operand1 lis)) (M_value_op (operand2 lis))))
      ((eq? (operator lis) '%) (remainder (M_value_op (operand1 lis)) (M_value_op (operand2 lis))))
      (else (error 'badoperation "Unknown operator")))))

;M_boolean_stuff

(define M_bool_op
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((not (list? lis)) lis)
      ((eq? (operator lis) '==) (eq? (M_bool_op (operand1 lis)) (M_bool_op (operand2 lis))))
      ((eq? (operator lis) '>=) (or (> (M_bool_op (operand1 lis)) (M_bool_op (operand2 lis)))
                                (eq? (M_bool_op (operand1 lis)) (M_bool_op (operand2 lis)))))
      ((eq? (operator lis) '<=) (or (< (M_bool_op (operand1 lis)) (M_bool_op (operand2 lis)))
                               (eq? (M_bool_op (operand1 lis)) (M_bool_op (operand2 lis)))))
      ((eq? (operator lis) '>) (> (M_bool_op (operand1 lis)) (M_bool_op (operand2 lis))))
      ((eq? (operator lis) '<) (< (M_bool_op (operand1 lis)) (M_bool_op (operand2 lis))))
      ((eq? (operator lis) '!=) (not (eq? (M_bool_op (operand1 lis)) (M_bool_op (operand2 lis)))))
      ((eq? (operator lis) '||) (or (M_bool_op (operand1 lis)) (M_bool_op (car (operand2 lis)))))
      ((eq? (operator lis) '&&) (and (M_bool_op (operand1 lis)) (M_bool_op (operand2 lis))))
      ((eq? (operator lis) '!) (not (M_bool_op (operand1 lis))))
      (else (M_value_op lis)))))

;abstraction for M_value_op and M_bool_op
(define operator car)
(define operand1 cadr)
(define operand2 caddr)


