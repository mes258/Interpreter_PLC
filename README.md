# Interpreter_PLC
PLC Interpreter Project by Vincent Portelli, Michael Smith, and Thomas Lerner.

## How to run the Interpreter
* Run `Interpreter3.rkt` and call `(interpret '"<filename>")` such as `(interpret '"code.txt")`. 
* The interpreter will return the value returned by the given code in the file. 


## To do: 
- [x] Part 2: 
	- [x] look at given try/catch and fix ours if needed
	- [x] make sure the rest of part 2 is perfect
- [x] Overall
	- [x] You should write a function called interpret that takes a filename 
	- [x] This calls parser with the filename 
	- [ ] Finally, it returns the proper value returned by main 
- [x] Make parser work with Dr. Racket
	- [x] As with the previous parser, this one is written for R5RS scheme, and you will need to comment/uncomment some lines to use it with racket.
- [x] Add function support 
	- [x] Add to MList:
	- [x] Base layer of state is global vars and functions
	- [x] M_value for calling a function
		- [x] create a function environment using the closure function on the current environment 
		- [x] evaluate each actual parameter in the current environment and bind it to the formal parameter in the function environment 
		- [x] interpret the body of the function with the function environment.
	- [x] M_bool into tail recursion form
	- [x] M_value into tail recursion form

```
function a(x, y) {          =>   (function a (x y) ((return (+ x y)))
  return x + y;
}

function main () {          =>   (function main () ((var x 10) (var y 15) (return (funcall gcd x y))))
  var x = 10;
  var y = 15;
  return gcd(x, y);
}```