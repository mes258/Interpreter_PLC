;Interpreter1 below:
;Vincent Portell, Michael Smith, Thomas Lerner

(require "simpleParser.scm")

;(define interpret
 ; (lambda (filename)
 ;   (parser filename)))

;Example list of statements:
;((var x) (= x 10) (var y (+ (* 3 x) 5))
;  (while (!= (% y x) 3) (= y (+ y 1)))
;  (if (> x y) (return x)
;    (if (> (* x x) y) (return (* x x))
;      (if (> (* x (+ x x)) y) (return (* x (+ x x)))
;        (return (- y 1))))))

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
      ((and (eq? (caar lis) 'var) (null? (cddar lis)))
       (M_list (cdr lis) (M_state_decl1 (fir lis) s)))
      ((eq? (type lis) 'var)
       (M_list (cdr lis) (M_state_decl2 (fir lis) (sec lis) s)))
      ((eq? (type lis) '=)
       (M_list (cdr lis) (M_state_assign (fir lis) (sec lis) s)))
      ((eq? (type lis) 'while)
       (M_list (cdr lis) s) (M_state_while (fir lis) (sec lis) s))
      ((eq? (type lis) 'return)
       (M_list (cdr lis) (M_state_return (fir lis) s)))
      ((and (eq? (type lis) 'if) (null? (cdddar lis)))
       (M_list (cdr lis) (M_state_if (fir lis) (sec lis) s)))
      ;((eq? (type lis) 'if)
       ;(cons (M_state_if_else (fir lis) (sec lis) (thr lis) s) (M_list (cdr lis) s)))
      (else lis)))) ; Parameters should be all fixed. Need to confirm M_list (car lis) works given defined lis above. 

;abstraction for M_list
;note: we are ignoring the first part of the line, such as "while" or "if". In those cases, (fir lis) refers to <condtion>.
(define type caar);type of call
(define fir cadar);First parameter
(define sec caddar);Second parameter
;(define thi cadddar);Third parameter


;need M_state for: 
;variable declaration 	(var variable) or (var variable value)
;assignment 	(= variable expression)
;return 	(return expression)
;if statement 	(if conditional then-statement optional-else-statement)
;while statement 	(while conditional body-statement)

;Need M_value for:
;math operators

;Need M_bool for:
;conditonals 

;M_state
(define M_state_decl1 ;add variable to state with value null
  (lambda (variable s)
    (cond
      ((null? s) (list (list variable) '(()) ))
      (else (cons (cons variable (car s)) (list (cons '() (cadr s)))))
      )))

(define M_state_decl2 ;add variable to state with value val
  (lambda (variable value s)
    (cond
      ((null? s) (list (list variable) (list value) ))
      (else (cons (cons variable (car s)) (list (cons value (cadr s)))))
      )))
(define M_state_assign ; set variable = exp in state
  (lambda (variable exp s)
    (cond
      ((null? s) (/ 1 0)) ; error (?)
      ((null? (car s)) (/ 1 0)) ;error ( :) )
      ((equal? variable (caar s))
       (list (car s) (cons (M_value_op exp) (cdadr s))))
      (else
       (cons (car s) (list (cons (caadr s) (cadr (M_state_assign variable exp (cons (cdar s) (list (cdadr s)))) )))
                  ;(list (cons (cdadr s) (cadr (
                  )))))      

(define M_state_while ;modify the state as the body says
  (lambda (condit body s) 
    (if (M_bool_op condit)
        (M_state_while condit body (M_list body s))
        s)))

(define M_state_return ;return exp
  (lambda (exp s)
    (cond
      ((null? exp) '())
      ((number? (M_bool_op exp)) (M_bool_op exp))
      ((eq? (caar s) 'exp) (cadr s))
      (else (M_state_return s (list (cdar) (cddr)))))))
;return needs to account for (return x) where x is in the state ; <- the above may work

(define M_state_if_else ;check the condition and modify s based on the value of condition 
  (lambda (condition then else s)
    (if (M_bool_op condition)
        (M_list then s)
        (M_list else s))))

(define M_state_if ; if true, modify based on then else do nothing
  (lambda (condition then s)
    (if (M_bool_op condition)
        (M_list then s)
        `())))


;M_value

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

;M_boolean

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

;(M_list '( (var x) (var y) (var z) (= y 7) ) '())

;(M_state_assign 'y 10 (M_state_decl1 'y (M_state_decl2 'x 7 '())))


;(M_state_assign 'x 69 '((a b x c)(5 () 2 ())) )

(M_state_assign 'y 7 (M_state_decl1 'z (M_state_decl1 'y (M_state_decl1 'x '()))))