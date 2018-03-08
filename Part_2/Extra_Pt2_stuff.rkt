;Extra non-cps functions and stuff to clean up the main file.

(define M_state_while ;modify the state as the body says
  (lambda (condition body s return throw break next) 
    (if (M_bool_op condition s)
        (M_state_while condition body (M_list body (M_list (list condition) s)))
        (M_list (list condition) s))))

(define M_state_assign ;set some variable in state equal to exp 
  (lambda (variable exp s)
    (cond
      ((null? s) s) ;if it's not there, don't set anything
      ((null? (car s)) (error variable "variable not defined"))
      ((equal? variable (caar s))
       (list (car s) (cons (M_value_op exp s) (cdadr s))))
      (else
       (cons (car s) (list (cons (caadr s) (cadr (M_state_assign variable exp (cons (cdar s) (list (cdadr s))))))))))))

(define M_assign_cps2;assign cps
  (lambda (variable exp s return throw break next)
    ((null? s) (return s))
    ((null? (car s)) (throw "not defined"))
    ((equal variable (caar s)) (return (list (car s) (cons (M_value_op exp s) (cdadr s)))))
    (else (M_assign_cps variable exp (cons (cdar s) (list (cdadr s))) (lambda (v) (return (cons (car s) (list (cons (caadr s) (cadr v))))) throw break)))))


    