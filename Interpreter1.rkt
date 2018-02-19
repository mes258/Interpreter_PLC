;Interpreter1 below:
;Vincent Portell, Michael Smith, Thomas Lerner
;EECS145 - Feb 19, 2018
(require "simpleParser.scm")

;To run:
;Call (runfile '"<filename>") where <filename> is any .txt file path. 

;go through the list of statements returned by interpreter
(define M_list
  (lambda (lis s)
    (cond
      ((null? lis) s)
      ((and (not (list? (car lis))) (null? (cdr lis))) s)
      ((and (eq? (type lis) 'var) (null? (cddar lis)))
       (M_list (cdr lis) (M_state_decl1 (fir lis) s)))
      ((eq? (type lis) 'var)
       (M_list (cdr lis) (M_state_decl2 (fir lis) (sec lis) s)))
      ((eq? (type lis) '=)
       (M_list (cdr lis) (M_state_assign (fir lis) (sec lis) s)))
      ((eq? (type lis) 'while)
       (M_list (cdr lis) (M_state_while (fir lis) (cddar lis) s)))
      ((eq? (type lis) 'return)
       (M_value_op (cadar lis) s))
      ((eq? (type lis) 'if)
       (if (null? (cdddar lis))
           (M_list (cdr lis) (M_state_if (ifcond (car lis)) (list (ifdo (car lis))) s))
           (M_list (cdr lis) (M_state_if_else (ifcond (car lis)) (list (ifdo (car lis))) (list (ifelsedo (car lis))) s))))
      ((or (eq? (type lis) '==)
           (eq? (type lis) '!=)
           (eq? (type lis) '>=)
           (eq? (type lis) '<=)
           (eq? (type lis) '>)
           (eq? (type lis) '<)
           (eq? (type lis) '+)
           (eq? (type lis) '-)
           (eq? (type lis) '/)
           (eq? (type lis) '%)
           (eq? (type lis) '*))
       (M_list (cdr lis) (M_list (list (operand2 (car lis))) (M_list (list (operand1 (car lis))) s))))
      (else s))))

;abstraction for M_list
;note: we are ignoring the first part of the line, such as "while" or "if". In those cases, (fir lis) refers to <condtion>.
(define type caar);type of call
(define fir cadar);First parameter
(define sec caddar);Second parameter
(define ifcond cadr)
(define ifdo caddr)
(define ifelsedo cadddr)

(define M_state  ;for if you want to get the state that results from a single statement
  (lambda (e s)
    (M_list (list e) s)))

;M_state for different operations
(define M_state_decl1 ;add variable to state with value null
  (lambda (variable s)
    (cond
      ((not (null? (varvalue variable s))) (error variable "already declared"))
      ((null? s) (list (list variable) list (noval)))
      (else (cons (cons variable (car s)) (list (cons noval (cadr s))))))))

(define M_state_decl2 ;add variable to state with value val
  (lambda (variable value s)
    (cond
      ((not (null? (varvalue variable s))) (error variable "already declared"))
      ((null? s) (list (list variable) (list (M_value_op value (M_state value s))) ))
      (else (cons (cons variable (car s)) (list (cons (M_value_op value s) (cadr s))))))))

(define M_state_assign ;set some variable in state equal to exp 
  (lambda (variable exp s)
    (cond
      ((null? s) s) ;if it's not there, don't set anything
      ((null? (car s)) (error variable "variable not defined"))
      ((equal? variable (caar s))
       (list (car s) (cons (M_value_op exp s) (cdadr s))))
      (else
       (cons (car s) (list (cons (caadr s) (cadr (M_state_assign variable exp (cons (cdar s) (list (cdadr s))))))))))))

(define M_state_while ;modify the state as the body says
  (lambda (condition body s) 
    (if (M_bool_op condition s)
        (M_state_while condition body (M_list body (M_list (list condition) s)))
        (M_list (list condition) s))))

(define M_state_return ;return exp
  (lambda (exp s)
    (cond
      ((null? exp) exp)
      ((number? (M_bool_op exp)) (M_bool_op exp))
      ((eq? (caar s) 'exp) (cadr s))
      (else (M_state_return s (list (cdar) (cddr)))))))

(define M_state_if_else ;check the condition and modify s based on the value of condition 
  (lambda (condition then else s)
    (if (M_bool_op condition s)
        (M_list then (M_list (list condition) s))
        (M_list else (M_list (list condition) s)))))

(define M_state_if ;if condition is true, modify based on then. Otherwise do nothing
  (lambda (condition then s)
    (if (M_bool_op condition (M_list (list condition) s))
        (M_list then (M_list (list condition) s))
        s)))

;M_value
(define M_value_op ;returns the value of an expression
  (lambda (lis s)
    (cond
      ((null? lis) lis)
      ((number? lis) lis)
      ((not (null? (varvalue lis s))) (varvalue lis s))
      ((not (list? lis)) (error lis "undefined variable"))
      ((null? (car lis)) (varvalue lis s))
      ((null? (cdr lis)) (M_value_op lis (car s)))
      ((eq? (operator lis) '+) (+ (M_value_op (operand1 lis) s) (M_value_op (operand2 lis) s)))
      ((eq? (operator lis) '-) (- (M_value_op (operand1 lis) s) (M_value_op (operand2 lis) s)))
      ((eq? (operator lis) '*) (* (M_value_op (operand1 lis) s) (M_value_op (operand2 lis) s)))
      ((eq? (operator lis) '/) (quotient (M_value_op (operand1 lis) s) (M_value_op (operand2 lis) s)))
      ((eq? (operator lis) '%) (remainder (M_value_op (operand1 lis) s) (M_value_op (operand2 lis) s)))
      ((eq? (operator lis) '=) (M_value_op (operand2 lis) (M_state_assign (operand1 lis) (operand2 lis) (M_state (operand2 lis) s)) ))
      ((or (eq? (operator lis) '==)
           (eq? (operator lis) '!=)
           (eq? (operator lis) '!)
           (eq? (operator lis) '||)
           (eq? (operator lis) '&&)
           (eq? (operator lis) '>)
           (eq? (operator lis) '<)
           (eq? (operator lis) '>=)
           (eq? (operator lis) '<=)) (if (M_bool_op lis s)
                                         'true
                                         'false))
      (else (error (operator lis) "Unknown operator")))))

(define varvalue ;gives the value of a variable given a state [if doesn't exist, gives null]
  (lambda (name s)
    (cond
      ((null? s) s)
      ((not (null? (car s)))
       (if(eq? (caar s) name )
          (caadr s)
          (varvalue name (cons (cdar s) (list(cdadr s))))))
      (else noval))))

(define noval '())

;M_boolean
(define M_bool_op ;Returns true or false given an expression and state
  (lambda (lis s)
    (cond
      ((null? lis) lis)
      ((eq? 'true lis) #t)
      ((eq? 'false lis) #f)
      ((not (list? lis)) lis)
      ((eq? (operator lis) '==) (eq? (M_value_op (operand1 lis) s) (M_value_op (operand2 lis) s)))
      ((eq? (operator lis) '>=) (or (> (M_value_op (operand1 lis) s) (M_value_op (operand2 lis) s))
                                    (eq? (M_value_op (operand1 lis) s) (M_value_op (operand2 lis) s))))
      ((eq? (operator lis) '<=) (or (< (M_value_op (operand1 lis) s) (M_value_op (operand2 lis) s))
                                    (eq? (M_value_op (operand1 lis) s) (M_value_op (operand2 lis) s))))
      ((eq? (operator lis) '>) (> (M_value_op (operand1 lis) s) (M_value_op (operand2 lis) s)))
      ((eq? (operator lis) '<) (< (M_value_op (operand1 lis) s) (M_value_op (operand2 lis) s)))
      ((eq? (operator lis) '!=) (not (eq? (M_value_op (operand1 lis) s) (M_value_op (operand2 lis) s))))
      ((eq? (operator lis) '||) (or (M_bool_op (operand1 lis) s) (M_bool_op (operand2 lis) s)))
      ((eq? (operator lis) '&&) (and (M_bool_op (operand1 lis) s) (M_bool_op (operand2 lis) s)))
      ((eq? (operator lis) '!) (not (M_bool_op (operand1 lis) s)))
      (else (M_value_op lis)))))

;abstraction for M_value_op and M_bool_op
(define operator car)
(define operand1 cadr)
(define operand2 caddr)

(define initState '())
;Code to Run
(define runfile
  (lambda (filename)
    (M_list (parser (build-path (current-directory) filename)) initState)))