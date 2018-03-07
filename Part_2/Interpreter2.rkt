;Interpreter2 below:
;Vincent Portell, Michael Smith, Thomas Lerner
;EECS145 - Feb 19, 2018
(require "simpleParser.scm")

;To run:
;Call (runfile '"<filename>") where <filename> is any .txt file path. 
;;To do: removed continue from parameters
;go through the list of statements returned by interpreter
(define M_list
  (lambda (lis s return throw break next)
    (cond
      ((null? lis) (return s))
      ((and (not (list? (car lis))) (null? (cdr lis))) (return s))
      ((eq? (type lis) 'var)
       (if (null? (cdddar lis))
           (M_state_decl1 (fir lis) s (lambda (v1) (M_list (cdr lis) v1 return throw break (lambda (v2) (next v2)))))
           (M_state_decl2 (fir lis) (sec lis) s (lambda (v1) (M_list (cdr lis) v1 return throw break (lambda (v2) (next v2)))))))
      ((eq? (type lis) '=)
       (M_state_assign (fir lis) (sec lis) s (lambda (v1) (M_list (cdr lis) v1 return throw break (lambda (v2) (next (v2))))))
      ((eq? (type lis) 'while)
       (M_state_while (fir lis) (sec lis) s return throw break (lambda (v1) (M_list (cdr lis) v1 return throw break (lambda (v2) (next v2)))))
      ((eq? (type lis) 'if)
       (if (null? (cdddar lis))
           (M_list (cdr lis) (M_state_if (ifcond (car lis)) (list (ifdo (car lis))) s return throw break) return throw break (lambda (v) (next v)))
           (M_list (cdr lis) (M_state_if_else (ifcond (car lis)) (list (ifdo (car lis))) (list (ifelsedo (car lis))) s return throw break) return throw break (lambda (v) (next v))))
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
       (M_list (list (operand1 (car lis))) s return throw break (lambda (v1) (M_list (list (operand2 (car lis))) v1 return throw break (lambda (v2) (M_list (cdr lis) v2 return throw break (lambda (v3) (next v3))))))))
      ;new stuff below
      ((eq? (type lis) 'break) (break s))
      ((eq? (type lis) 'throw) (throw (M_value_op (fir lis) (removeStateFrame s) next)))
      ((eq? (type lis) 'continue) (next s)
      ((eq? (type lis) 'return) (return (M_value_op (fir lis) s next) s))
      ((eq? (type lis) 'begin) (M_block (cdr lis) s return throw break))
      ((eq? (type lis) 'try) (M_state_try (fir lis) (sec lis) (thr lis) s return throw break (lambda (v) (next v))))
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
  (lambda (e s return throw break next)
    (M_list (list e) s return throw break next)))

(define M_block ;evaluate a block of code
  (lambda (lis s return throw break next)
    (M_list lis (addStateFrame s) return throw break next)))

(define addStateFrame
  (lambda (s)
    (cons '() s)))

(define removeStateFrame
  (lambda (s)
    (cdr s)))

;M_state for different operations
(define M_state_decl1 ;add variable to state with value null
  (lambda (var s next)
    (cond
      ((null? s) (next (list (list (list var) (list noval)))))
      ((null? (car s)) (next (cons (list (list var) (list noval)) (car s))))
      ((null? (checklayer var (car s))) (next (cons (cons (cons var (caar s)) (list (cons noval (cadar s)))) (cdr s))))
      (else (error var "Already declared in this block")))))

(define M_state_decl2 ;add variable to state with value val
  (lambda (var val s next)
    (cond
      ((null? s) (next (list (list (list var) (list val)))))
      ((null? (car s)) (next (cons (list (list var) (list val)) (car s))))
      ((null? (checklayer var (car s))) (next (cons (cons (cons var (caar s)) (list (cons val (cadar s)))) (cdr s))))
      (else (error var "Already declared in this block")))))

(define M_assign_cps;assign cps
  (lambda (variable exp s return throw break next)
    ;add code here
    ))

(define return ;return the value 
  (lambda (var state)
    (varvalue var state)))

(define M_while_cps;while cps
  (lambda (condit body s return throw break next)
    (if (M_bool_op condition s next)
        (M_list (list condition) s return throw break (lambda (v1) (M_list body v1 return throw break (lambda (v2) (M_state_while condition body v2 return throw break (lambda (v3) (next v3)))))))
        (M_list (list condition) s return throw break (lambda (v1) (next v1))))))

(define M_state_if_else ;check the condition and modify s based on the value of condition 
  (lambda (condition then else s return throw break next)
    (if (M_bool_op condition s next)
        (M_list (list condition) s return throw break (lambda (v1) (M_list then v1 return throw break (lambda (v2) (next v2)))))
        (M_list (list condition) s return throw break (lambda (v1) (M_list else v1 return throw break (lambda (v2) (next v2))))))))

(define M_state_if ;if condition is true, modify based on then. Otherwise do nothing
  (lambda (condition then s return throw break next)
    (if (M_list (list condition) s return throw break (lambda (v1) (M_bool_op condition v1 (lambda (v2) (next v2)))))
        (M_list (list condition) s return throw break (lambda (v1) (M_list then v1 return throw break (lambda (v2) (next v2))))))))

(define M_state_try;Should work
  (lambda (body catch finally s return throw break next)
    (M_list (cdr finally) (M_list body s (lambda (v s))
                                  (lambda (v) (M_list (thr catch) (M_state_decl2 (car (sec catch)) v (addStateFrame s) return break throw) return break throw)) break) return throw break)))
;M_value
(define M_value_op ;returns the value of an expression
  (lambda (lis s next)
    (cond
      ((null? lis) (next lis))
      ((number? lis) (next lis))
      ((not (null? (varvalue lis s))) (next (varvalue lis s)))
      ((not (list? lis)) (error lis "undefined variable"))
      ((null? (car lis)) (next (varvalue lis s)))
      ((null? (cdr lis)) (next (M_value_op lis (car s))))
      ((eq? (operator lis) '+) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (+ v1 v2)))))))
      ((eq? (operator lis) '-) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (- v1 v2)))))))
      ((eq? (operator lis) '*) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (* v1 v2)))))))
      ((eq? (operator lis) '/) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (quotient v1 v2)))))))
      ((eq? (operator lis) '%) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (remainder v1 v2)))))))
      ((eq? (operator lis) '=) (M_state (operand2 lis) s return throw break (lambda (v1) (M_state_assign (operand1 lis) (operand2 lis) v1 return throw break (lambda (v2) (M_value_op (operand2 lis) v2 (lambda (v3) (next v3))))))))
      ((or (eq? (operator lis) '==)
           (eq? (operator lis) '!=)
           (eq? (operator lis) '!)
           (eq? (operator lis) '||)
           (eq? (operator lis) '&&)
           (eq? (operator lis) '>)
           (eq? (operator lis) '<)
           (eq? (operator lis) '>=)
           (eq? (operator lis) '<=)) (if (M_bool_op lis s next)
                                         (next 'true)
                                         (next 'false)))
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
  (lambda (lis s next)
    (cond
      ((null? lis) (next lis))
      ((eq? 'true lis) (next #t))
      ((eq? 'false lis) (next #f))
      ((not (list? lis)) (next lis))
      ((eq? (operator lis) '==) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (eq? v1 v2)))))))
      ((eq? (operator lis) '>=) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (or (> v1 v2) (eq? v1 v2))))))))
      ((eq? (operator lis) '<=) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (or (< v1 v2) (eq? v1 v2))))))))
      ((eq? (operator lis) '>) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (> v1 v2)))))))
      ((eq? (operator lis) '<) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (< v1 v2)))))))
      ((eq? (operator lis) '!=) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (not (eq? v1 v2))))))))
      ((eq? (operator lis) '||) (M_bool_op (operand1 lis) s (lambda (v1) (M_bool_op (operand2 lis) s (lambda (v2) (next (or v1 v2)))))))
      ((eq? (operator lis) '&&) (M_bool_op (operand1 lis) s (lambda (v1) (M_bool_op (operand2 lis) s (lambda (v2) (next (and v1 v2)))))))
      ((eq? (operator lis) '!) (M_bool_op (operand1 lis) s (lambda (v1) (next (not v1)))))
      (else (M_value_op lis s next)))))

;abstraction for M_value_op and M_bool_op
(define operator car)
(define operand1 cadr)
(define operand2 caddr)

(define initState '())
;Code to Run
(define interpret
  (lambda (filename)
    (M_list (parser (build-path (current-directory) filename)) initState return (lambda (v s) (error "Something is wrong; throw was called" v)) (lambda (v) (error "Not a valid break")))))
