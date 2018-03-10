;Interpreter 2 below:
;Vincent Portell, Michael Smith, Thomas Lerner
;EECS345 - March 9, 2018
(require "simpleParser.scm")

;To run:
;Call (interpret '"<filename>") where <filename> is any .txt file path.

;go through the list of statements returned by interpreter
(define M_list
  (lambda (lis s return throw break next)
    (cond
      ((null? lis) (next s))
      ((and (not (list? (car lis))) (null? (cdr lis))) (next s))
      ((eq? (type lis) 'var)
       (if (null? (cddar lis))
           (M_state_decl1 (fir lis) s (lambda (v1) (M_list (cdr lis) v1 return throw break (lambda (v2) (next v2)))))
           (M_value_op (sec lis) s (lambda (value) (M_state_decl2 (fir lis) value s (lambda (v1) (M_list (cdr lis) v1 return throw break next)))))))
      ((eq? (type lis) '=)
       (M_value_op (sec lis) s (lambda (v1) (M_state_assign (fir lis) v1 s (lambda (v2) (M_list (cdr lis) v2 return throw break (lambda (v3) (next v3))))))))
      ((eq? (type lis) 'while)
       (M_state_while (fir lis) (sec lis) s return throw (lambda (vx) (M_list (cdr lis) vx return throw break next)) (lambda (v1) (M_list (cdr lis) v1 return throw break next))))
      ((eq? (type lis) 'if)
       (if (null? (cdddar lis))
           (M_state_if (ifcond (car lis)) (list (ifdo (car lis))) s return throw break (lambda (v1) (M_list (cdr lis) v1 return throw break (lambda (v2) (next v2)))))
           (M_state_if_else (ifcond (car lis)) (ifdo (car lis)) (ifelsedo (car lis)) s return throw break (lambda (v1) (M_list (cdr lis) v1 return throw break next)))))
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
           (eq? (type lis) '*)
           (eq? (type lis) '||)
           (eq? (type lis) '&&))
           ;(eq? (type lis) '!))
       (M_list (list (operand1 (car lis))) s return throw break (lambda (v1) (M_list (list (operand2 (car lis))) v1 return throw break (lambda (v2) (M_list (cdr lis) v2 return throw break (lambda (v3) (next v3))))))))
      ((eq? (type lis) '!) (M_list (list (operand1 (car lis))) s return throw break (lambda (v1) (M_list (cdr lis) v1 return throw break next))))
      ((eq? (type lis) 'break) (break s))
      ((eq? (type lis) 'throw) (throw (fir lis) s))
      ((eq? (type lis) 'continue) (next s))
      ((eq? (type lis) 'return) (return (fir lis) s (lambda (v) v)))
      ((eq? (type lis) 'begin) (M_block (cdar lis) s return throw break (lambda (v1) (M_list (cdr lis) v1 return throw break next))))
      ((eq? (type lis) 'try) (M_state_try (fir lis) (sec lis) (thr lis) s return throw break (lambda (v) (M_list (cdr lis) v return throw break next) )))
      (else s))))

;abstraction for M_list
;note: we are ignoring the first part of the line, such as "while" or "if". In those cases, (fir lis) refers to <condtion>.
(define type caar);type of call
(define fir cadar);First parameter
(define sec caddar);Second parameter
(define thr
  (lambda (l)
  (car (cdddar l))));Third parameter
(define ifcond cadr)
(define ifdo caddr)
(define ifelsedo cadddr)

;M_list helper functions
(define M_state  ;for if you want to get the state that results from a single statement
  (lambda (e s return throw break next)
    (M_list (list e) s return throw break next)))

(define M_block ;evaluate a block of code
  (lambda (lis s return throw break next)
    (M_list lis (addStateFrame s) return throw break (lambda (v) (next (removeStateFrame v))))))

(define addStateFrame ;Add an empty state when entering a new block
  (lambda (s)
    (cons '() s)))

(define removeStateFrame ;Remove the top state when leaving a block
  (lambda (s)
    (cdr s)))

;M_state for different operations
(define M_state_decl1 ;add variable to state with value null
  (lambda (var s next)
    (cond
      ((null? s) (next (list (list (list var) (list noval)))))
      ((null? (car s)) (next (cons (list (list var) (list noval)) (cdr s))))
      ((null? (checklayer var (car s))) (next (cons (cons (cons var (caar s)) (list (cons noval (cadar s)))) (cdr s))))
      (else (error var "Already declared in this block")))))

(define M_state_decl2 ;add variable to state with value val
  (lambda (var val s next)
    (cond
      ((null? s) (next (list (list (list var) (list val)))))
      ((null? (car s)) (next (cons (list (list var) (list val)) (cdr s))))
      ((null? (checklayer var (car s))) (next (cons (cons (cons var (caar s)) (list (cons val (cadar s)))) (cdr s))))
      (else (error var "Already declared in this block")))))

(define M_state_assign ;assign cps
  (lambda (var expr s next)
    (cond
      ((null? s) (error var "Not declared yet"))
      ((null? (car s)) (M_state_assign var expr (cdr s) (lambda (v) (next(cons (car s) v)))))
      ((null? (caar s)) (M_state_assign var expr (cdr s) (lambda (v) (next (cons (car s) v)))))
      ((equal? var (caaar s)) (M_value_op expr s (lambda (v) (next (cons (list (caar s) (cons v (cdadar s))) (cdr s))))))
      (else (M_state_assign var expr (cons (list (cdaar s) (cdadar s)) (cdr s)) (lambda (v) (next (cons (list (cons (caaar s) (caar v)) (cons (caadar s) (cadar v))) (cdr v)))))))))

(define return ;return the value 
  (lambda (var state next)
    (M_value_op var state next)))

(define M_state_while ;while in cps form
  (lambda (condition body s return throw break next)
    (M_list (list condition) s return throw break (lambda (v1) (M_bool_op condition s (lambda (v2) (if v2
                                                                                                        (M_list (list condition) s return throw break (lambda (v3) (M_list (list body) v3 return throw break (lambda (v4) (M_state_while condition body v4 return throw break next)))))
                                                                                                        (M_list (list condition) s return throw break next))))))))

(define M_state_if_else ;check the condition and modify s based on the value of condition 
  (lambda (condition then else s return throw break next)
    (M_list (list condition) s return throw break (lambda (v1) (M_bool_op condition v1 (lambda (v2) (if v2
                                                                                                        (M_list (list condition) v1 return throw break (lambda (v3) (M_list (list then) v3 return throw break next)))
                                                                                                        (M_list (list condition) v1 return throw break (lambda (v3) (M_list (list else) v3 return throw break next))))))))))

(define M_state_if ;if condition is true, modify based on then. Otherwise do nothing
  (lambda (condition then s return throw break next)
    (M_list (list condition) s return throw break (lambda (v1) (M_bool_op condition v1 (lambda (v2) (if v2
                                                                                                        (M_list then v1 return throw break next)
                                                                                                        (next v1))))))))

(define M_state_try ;Try, catch, finally
  (lambda (body catch finally s return throw break next)
    (if (not (null? finally))
        (M_list body (addStateFrame s)
                (lambda (v1 s1) (M_list finally s1 return throw break (return v1 s1)))
                (lambda (v1 s1) (M_value_op v1 s1 (lambda (value)
                                                (M_state_decl2 (caadr catch) value s1 (lambda (s2)
                                                  (M_list (caddr catch) s2 return throw break (lambda (s3)
                                                    (M_list (cadr finally) s3 return throw break (lambda (s4)
                                                      (next (removeStateFrame s4)))))))))))
                (lambda (s1) (M_list (cadr finally) s1 return throw break (lambda (s2) (next (removeStateFrame s2)))))
                (lambda (s1) (M_list (cadr finally) s1 return throw break (lambda (s2) (next (removeStateFrame s2))))))
        (M_list body (addStateFrame s)
                (lambda (v1 s1) (M_list finally s1 return throw break (return v1 s1)))
                (lambda (v1 s1) (M_value_op v1 s1 (lambda (value)
                                                    (M_state_decl2 (caadr catch) value s1 (lambda (s2)
                                                                                          (M_list (caddr catch) s2 return throw break (lambda (s3)
                                                                                                                                      (M_list finally s3 return throw break (lambda (s4)
                                                                                                                                                                                     (next (removeStateFrame s4)))))))))))
                (lambda (s1) (M_list finally s1 return throw break (lambda (s2) (next (removeStateFrame s2)))))
                (lambda (s1) (M_list finally s1 return throw break (lambda (s2) (next (removeStateFrame s2)))))))))
                            
;M_value
(define M_value_op ;returns the value of an expression
  (lambda (lis s next)
    (cond
      ;((not (list? lis)) (M_value_op (list lis) s next))
      ((null? lis) (next lis))
      ((number? lis) (next lis))
      ((eq? 'true lis) (next 'true))
      ((eq? 'false lis) (next 'false))
      ((eq? #t lis) (next 'true))
      ((eq? #f lis) (next 'false))
      ((not (null? (varvalue lis s))) (next (varvalue lis s)))
      ((not (list? lis)) (error s "undefined variable"))
      ((null? (car lis)) (next (varvalue lis s)))
      ((eq? (operator lis) '+) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (+ v1 v2)))))))
      ((eq? (operator lis) '-) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (- v1 v2)))))))
      ((eq? (operator lis) '*) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (* v1 v2)))))))
      ((eq? (operator lis) '/) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (quotient v1 v2)))))))
      ((eq? (operator lis) '%) (M_value_op (operand1 lis) s (lambda (v1) (M_value_op (operand2 lis) s (lambda (v2) (next (remainder v1 v2)))))))
      ((eq? (operator lis) '=) (M_state (operand2 lis) s return throw break (lambda (v1) (M_state_assign (operand1 lis) (operand2 lis) v1 return throw break (lambda (v2) (M_value_op (operand2 lis) v2 next))))))
      ((or (eq? (operator lis) '==)
           (eq? (operator lis) '!=)
           (eq? (operator lis) '!)
           (eq? (operator lis) '||)
           (eq? (operator lis) '&&)
           (eq? (operator lis) '>)
           (eq? (operator lis) '<)
           (eq? (operator lis) '>=)
           (eq? (operator lis) '<=)) (M_bool_op lis s (lambda (v) (if v (next 'true) (next 'false)))))
      (else (error (operator lis) "Unknown operator")))))

;M_value helper functions
(define varvalue ;gives the value of a variable given a state [if doesn't exist, gives null]
  (lambda (name s)
    (cond
      ((null? s) s)
      ((null? (car s)) (varvalue name (cdr s)))
      ((null? (caar s)) (varvalue name (cdr s)))
      ((equal? name (caaar s)) (caadar s))
      (else (varvalue name (cons (list (cdaar s) (cdadar s)) (cdr s)))))))

(define checklayer ;varvalue within a single frame
  (lambda (name lis)
    (cond
      ((null? lis) lis)
      ((not (null? (car lis)))
       (if(eq? (caar lis) name )
          (caadr lis)
          (checklayer name (cons (cdar lis) (list(cdadr lis))))))
      (else noval))))

;If the state has no value, it is an empty list
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

;The initial state is a null list
(define initState '())

;Code to Run - run this by calling: (interpret "code.txt") or with the path to some other file
(define interpret
  (lambda (filename)
    (M_list (parser (build-path (current-directory) filename)) initState return (lambda (v s) (error "Something is wrong; throw was called" v)) (lambda (v) (error "Not a valid break")) (lambda (v) v))))