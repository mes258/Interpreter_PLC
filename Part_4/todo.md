# todo

- [ ] when instances are constructed (create-instance-closure), they need to get variables from their parent class also. This should look like (for an example where B extends A): 

(B (B-dynamic-vars B-static-vars A-dynamic-vars A-static-vars))

(
	((main) (#&(() ((var test (new C)) (return (funcall (dot (new C) m))))))) 
	(() ()) 

	((C B A) 
	(
	 #&(((m2 m) (#&(() ((return (funcall (dot this m2))))) #&(() ((return (+ x y)))))) (() ()) ((y x) (#&1 #&2)) (() ()) ())
	 #&(((m2 m) (#&(() ((return (funcall (dot super m))))) #&(() ((return (+ (+ x y) z)))))) (() ()) ((z y) (#&22 #&3)) (() ()) ((extends A))) 
	 #&(((m) (#&(() ((return (funcall (dot super m))))))) ((main) (#&(() ((var test (new C)) (return (funcall (dot (new C) m))))))) ((w y) (#&222 #&4)) (() ()) ((extends B)))))
)


- [ ] make a function (get-intance-names-and-values {evaluated instance ex: 

'(B (B-dynamic-vars B-static-vars A-dynamic-vars A-static-vars))} 

{environment} ) => 

(
(B-dynamic-var-names B-static-var-names B-dynamic-method-names B-static-method-names A-dynamic-var-names A-static-var-names A-dynamic-method-names A-static-method-names)

(B-dynamic-var-vals B-static-var-vals B-dynamic-method-vals B-static-method-vals A-dynamic-var-vals A-static-var-vals A-dynamic-method-vals A-static-method-vals)
). 


This will take a lot of work/helper functions... idk