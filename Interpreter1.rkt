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
      ((and (eq? 'var (car (car lis))) (null? (cdr (cdr (car lis))))) (cons (M_state_decl1 (car lis)) (M_statement_list (cdr lis) s)))
      ((eq? 'var (car (car lis))) (cons (M_state_decl2 (car lis)) (M_statement_list (cdr lis) s)))
      ((eq? 'while (car (car lis))) (cons (M_state_while (car lis)) (M_statement_list (cdr lis) s)))
      ((eq? 'return (car (car lis))) (cons (M_state_return (car lis)) (M_statement_list (cdr lis) s)))
      ((eq? 'if (car (car lis))) (cons (M_state_if (car lis)) (M_statement_list (cdr lis) s)))
      (else s))))

;abstraction for M_statement_list
(define type caar)

      ;more sutff here

    
   ; (if (null? slist)
   ;     s
   ;     (cons (M_statement_list (cdr list) s) (M_statement_list (car slist) s)))))

;need M_bool, M_state, M_value for:
; variable declaration 	(var variable) or (var variable value)
;assignment 	(= variable expression)
;return 	(return expression)
;if statement 	(if conditional then-statement optional-else-statement)
;while statement 	(while conditional body-statement)

;M_State_stuff
(define M_state_decl1
  (lambda (var variable)
    (cond
      )))

(define M_state_decl2
  (lambda (var variable value)
    (cond
      )))

(define M_state_while
  (lambda (while condit body) 
    (cond
      )))

(define M_state_return
  (lambda (return exp)
    (cond
      ((null? exp) '())
      (else (M_bool_op exp)))))

(define M_state_if
  (lambda (condition then else s)
    (if (M_bool_op condition s)
        (M_state then s)
        (M_state else s))))


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
      ((not? (list? lis)) lis)
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


