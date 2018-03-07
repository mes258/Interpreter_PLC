# Interpreter_PLC
PLC Interpreter Project by Vincent Portelli, Michael Smith, and Thomas Lerner.

## How to run the Interpreter
* The interpreter will return the value returned by the given code in the file. 

## To do: 

- [ ] Make M_state functions tail recursive: 
	- [x] M_list - make sure this is call all functions in cps form
	- [x] M_state
	- [x] M_block
	- [x] M_state_decl1
	- [x] _decl2
	- [ ] _assign
	- [x] _while
	- [x] _if_else 
	- [x] _if 
	- [x] _bool (need to modify format in the if statements, while and other places its called.)
	- [x] _value (need to modify format in the if statements, while and other places its called.)

- [x] Return: lambda function: change return to accept two inputs, the variable name (what is being returned) and the state. The base return (initially passed into M_list) should just be (varvalue var s)
- [ ] Break: lambda function
- [x] Continue: not a lambda function. if a continue is hit in the block, stop evaluating the block and just return the state
- [x] Throw: lambda function
- [x] Try catch: lambda function
