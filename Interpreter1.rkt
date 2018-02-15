;Interpreter1 below:
;Vincent Portell, Michael Smith, Thomas Lerner

(require "simpleParser.scm")

;(define interpret
 ; (lambda (filename)
 ;   (parser filename)))

;Example list of statements:
;((var x) (= x 10) (var y (+ (* 3 x) 5))
;(while (!= (% y x) 3) (= y (+ y 1)))
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
      ((and (eq? (type lis) 'var) (null? (caddr lis)))
       (cons (M_state_decl1 (fir lis) s) (M_list (cdr lis) s)))
      ((eq? (type lis) 'var)
       (cons (M_state_decl2 (fir lis) (sec lis) s) (M_list (cdr lis) s)))
      ((eq? (type lis) '=)
       (cons (M_state_assign (fir lis) (sec lis) s) (M_list (cdr lis) s)))
      ((eq? (type lis) 'while)
       (cons (M_state_while (fir lis) (sec lis) s) (M_list (cdr lis) s)))
      ((eq? (type lis) 'return)
       (cons (M_state_return (fir lis) s) (M_list (cdr lis) s)))
      ((and (eq? (type lis) 'if) (null? (cdddar lis)))
       (cons (M_state_if (fir lis) (sec lis) s) (M_list (cdr lis) s)))
      ((eq? (type lis) 'if)
       (cons (M_state_if_else (fir lis) (sec lis) (thr lis) s) (M_list (cdr lis) s)))
      (else lis)))) ; Parameters should be all fixed. Need to confirm M_list (car lis) works given defined lis above. 

;abstraction for M_list
;note: we are ignoring the first part of the line, such as "while" or "if". In those cases, (fir lis) refers to <condtion>.
(define type caar);type of call
(define fir cadr);First parameter
(define sec caddr);Second parameter
(define thi cadddr);Third parameter


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
      ((null? s) (list (variable) ()))
      (else (cons (variable (car s)))))))

(define M_state_decl2 ;add variable to state with value val
  (lambda (variable value s)
    (cond
      ((null? s) (list (variable) (value)))
      (else (list (cons variable (car s)) (cons value (cadr s)))))))

(define M_state_assign ; set variable = exp in state
  (lambda (variable exp s)
    (cond
      ((null? s) s)
      ((eq? 'variable (caar s)) (cons exp (cddr lis)))
      (else (M_state_assign variable exp (list (cdar s) (cddr s)))))))      

(define M_state_while ;modify the state as the body says
  (lambda (condit body s) 
    (if (M_bool_op condit)
        (M_state_while condit (cdr body) (M_list (car body) s))
        ;how to modify condit when state changes? helper function maybe?-> takes in body and condit and sees if condit changes?
        s)))
(define while_helper
  (lambda (con body)
    (cond
      (()))))

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


