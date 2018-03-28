# Interpreter_PLC
PLC Interpreter Project by Vincent Portelli, Michael Smith, and Thomas Lerner.

## How to run the Interpreter
* Run `Interpreter3.rkt` and call `(interpret '"<filename>")` such as `(interpret '"code.txt")`. 
* The interpreter will return the value returned by the given code in the file. 


## To do: 
- [ ] Part 2: 
	- [ ] look at given try/catch and fix ours if needed
	- [ ] make sure the rest of part 2 is perfect
- [ ] Overall
	- [ ] You should write a function called interpret that takes a filename 
	- [ ] This calls parser with the filename 
	- [ ] Then evaluates the parse tree returned by parser
	- [ ] Finally, it returns the proper value returned by main 
- [ ] Make parser work with Dr. Racket
	- [ ] As with the previous parser, this one is written for R5RS scheme, and you will need to comment/uncomment some lines to use it with racket.
- [ ] Add function support 
	- [ ] Add to MList: 
```
function a(x, y) {          =>   (function a (x y) ((return (+ x y)))
  return x + y;
}

function main () {          =>   (function main () ((var x 10) (var y 15) (return (funcall gcd x y))))
  var x = 10;
  var y = 15;
  return gcd(x, y);
}

	- [ ] Base layer of state is global vars and functions
	- [ ] M_value for calling a function
		- [ ] create a function environment using the closure function on the current environment 
		- [ ] evaluate each actual parameter in the current environment and bind it to the formal parameter in the function environment 
		- [ ] interpret the body of the function with the function environment.
	- [x] M_bool into tail recursion form
	- [x] M_value into tail recursion form