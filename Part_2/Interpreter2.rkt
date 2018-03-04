;Interpreter2 below:
;Vincent Portell, Michael Smith, Thomas Lerner
;EECS145 - Feb 19, 2018
(require "simpleParser.scm")

;To run:
;Call (runfile '"<filename>") where <filename> is any .txt file path. 
;;To do: removed continue from parameters
;go through the list of statements returned by interpreter
(define M_list
  (lambda (lis s return throw break)
    (cond
      ((null? lis) s)
      ((and (not (list? (car lis))) (null? (cdr lis))) s)
      ((eq? (type lis) 'var)
       (if (null? (cdddar lis))
                  (M_list (cdr lis) (M_state_decl1 (fir lis) s return throw break) return throw break)
                  (M_list (cdr lis) (M_state_decl2 (fir lis) (sec lis) s return throw break) return throw break)))
      ((eq? (type lis) '=)
       (M_list (cdr lis) (M_state_assign (fir lis) (sec lis) s return throw break) return throw break))
      ((eq? (type lis) 'while)
       (M_list (cdr lis) (M_state_while (fir lis) (cddar lis) s return throw break) return throw break))
      ((eq? (type lis) 'if)
       (if (null? (cdddar lis))
           (M_list (cdr lis) (M_state_if (ifcond (car lis)) (list (ifdo (car lis))) s return throw break) return throw break)
           (M_list (cdr lis) (M_state_if_else (ifcond (car lis)) (list (ifdo (car lis))) (list (ifelsedo (car lis))) s) return throw break) return throw break))
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
      ;new stuff below
      ((eq? (type lis) 'break) (break s))
      ((eq? (type lis) 'throw) (throw (M_value_op (fir lis) (removeStateFrame s))))
      ((eq? (type lis) 'continue) s)
      ((eq? (type lis) 'return) (M_value_op (cadar lis) s return throw break))
      ((eq? (type lis) 'begin) (M_block (cdr lis) s return throw break))
      ((eq? (type lis) 'try) (M_state_try (fir lis) (sec lis) (thr lis) s return throw break))
      (else s))))

;abstraction for M_list
;note: we are ignoring the first part of the line, such as "while" or "if". In those cases, (fir lis) refers to <condtion>.
(define type caar);type of call
(define fir cadar);First parameter
(define sec caddar);Second parameter
(define thr cadddar);Third parameter
(define ifcond cadr)
(define ifdo caddr)
(define ifelsedo cadddr)

(define M_state  ;for if you want to get the state that results from a single statement
  (lambda (e s return throw break)
    (M_list (list e) s return throw break)))

(define M_block ;evaluate a block of code
  (lambda (s lis return throw break)
    (M_list s lis return throw break)))

(define addStateFrame
  (lambda (s)
    (cons '() s)))

(define removeStateFrame
  (lambda (s)
    (cdr s)))

;M_state for different operations
(define M_state_decl1 ;add variable to state with value null
  (lambda (var s)
    (cond
      ((null? s) (list (list (list var) (list noval))))
      ((null? (car s)) (cons (list (list var) (list noval)) (car s)))
      ((null? (checklayer var (car s))) (cons (cons (cons var (caar s)) (list (cons noval (cadar s)))) (cdr s)))
      (else (error var "Already declared in this block")))))

(define M_state_decl2 ;add variable to state with value val
  (lambda (var val s)
    (cond
      ((null? s) (list (list (list var) (list val))))
      ((null? (car s)) (cons (list (list var) (list val)) (car s)))
      ((null? (checklayer var (car s))) (cons (cons (cons var (caar s)) (list (cons val (cadar s)))) (cdr s)))
      (else (error var "Already declared in this block")))))


(define M_state_assign ;set some variable in state equal to exp 
  (lambda (variable exp s return throw break)
    (cond
      ((null? s) s) ;if it's not there, don't set anything
      ((null? (car s)) (error variable "variable not defined"))
      ((equal? variable (caar s))
       (list (car s) (cons (M_value_op exp s) (cdadr s))))
      (else
       (cons (car s) (list (cons (caadr s) (cadr (M_state_assign variable exp (cons (cdar s) (list (cdadr s))))))))))))


(define M_assign_cps;assign cps
  (lambda (variable exp s return throw break)
    ((null? s) (return s))
    ((null? (car s)) (throw "not defined"))
    ((equal variable (caar s)) (return (list (car s) (cons (M_value_op exp s) (cdadr s)))))
    (else (M_assign_cps variable exp (cons (cdar s) (list (cdadr s))) (lambda (v) (return (cons (car s) (list (cons (caadr s) (cadr v))))) throw break)))))
    

(define M_state_while ;modify the state as the body says
  (lambda (condition body s return throw break) 
    (if (M_bool_op condition s)
        (M_state_while condition body (M_list body (M_list (list condition) s)))
        (M_list (list condition) s))))

(define M_while_cps;while cps
  (lambda (condit body s return throw break)
    (if (M_bool_op condit s)
        (M_list (list condit) s (lambda (v1) (M_list body v1 (lambda (v2) (M_while_cps condit body v2 (lambda (v3) (return v3)) throw break)) throw break)) throw break)
        (M_list (list condit) s (lambda (v) v) throw break))))


(define M_state_return ;return exp
  (lambda (exp s return throw break)
    (cond
      ((null? exp) exp)
      ((number? (M_bool_op exp)) (M_bool_op exp))
      ((eq? (caar s) 'exp) (cadr s))
      (else (M_state_return s (list (cdar) (cddr)))))))

(define M_state_if_else ;check the condition and modify s based on the value of condition 
  (lambda (condition then else s return throw break)
    (if (M_bool_op condition s)
        (M_list then (M_list (list condition) s))
        (M_list else (M_list (list condition) s)))))

(define M_state_if ;if condition is true, modify based on then. Otherwise do nothing
  (lambda (condition then s return throw break)
    (if (M_bool_op condition (M_list (list condition) s))
        (M_list then (M_list (list condition) s))
        s)))

;(try body (catch (e) body) (finally body))
(define M_state_try
  (lambda (body catch finally s return throw break)
    (M_list (cdr finally) (M_list body s return
                                  (lambda (v) (M_list (thr catch) (M_state_decl2 (car (sec catch)) v (addStateFrame s) return break throw) return break throw)) break) return throw break)))

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
      ((null? (car s)) (varvalue name (cdr s)))
      ((null? (checklayer name (car s))) (varvalue name (cdr s)))
      (else (checklayer name (car s))))))

(define checklayer
  (lambda (name lis)
    (cond
      ((null? lis) lis)
      ((not (null? (car lis)))
       (if(eq? (caar lis) name )
          (caadr lis)
          (checklayer name (cons (cdar lis) (list(cdadr lis))))))
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

(define return
  (lambda (var state)
    (varvalue var state)))

(define initState '())
;Code to Run
(define interpret
  (lambda (filename)
    (M_list (parser (build-path (current-directory) filename)) initState return (lambda (v) (error "Not a valid continue")) (lambda (v) (error "Something is wrong; throw was called" v)) (lambda (v) (error "Not a valid break")))))
