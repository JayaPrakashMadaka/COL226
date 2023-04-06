# VMC-WHILE
VMC- WHILE
# Assignment - 4
###### submission 2020CS10356  (Madaka Jaya Prakash)
## AST for WHILE language in SML and VMC Machine.
##### Files in the directory :
1. while.lex
2. while.yacc
3. while_ast.sml
4. loader.sml
5. program.txt
6. Stack.sml
7. VMC.sml
##### Running Commands :
ml-lex while.lex
ml-yacc while.yacc
sml loader.sml

These are the commands to get AST Tree in the terminal
sml VMC.sml

This command gives the VMC machine.

It has two functions execute() and initiate()

##### Files generated :
1. while.lex.sml
2. while.yacc.desc
3. while.yacc.sig
4. while.yacc.sml
5. 


To increase print depth print Control.Print.printDepth := 100;
To increase print length print Control.Print.printLength := 100;
##### overview of assignment :
The program code is written in program.txt file which should be written in WHILE language in pg : 202 of Hyper Notes.
lexical errors and parsing errors are thrown when required.
The SML program prints CalcParse.result value which is an AST of the given code in program.txt.
A data type tree is defined in this SML code , which is a binary tree which has node values as strings.

#### Signature and Structures Implemented :
#### Task : 1
Stack : signature given in assignment is made
FunStack :> Stack structure is designed based on signature defined.
The structure FunStack is in Stack.sml file.
which is task 1 of the assignment

#### Task : 2

Memory Array is initialized the design of symbol table is made in :
Memory is an array of (string * string * string )
the first string contains the variable name eg : "x", "y","a"
the second string is its value if declared(SET) : "1" ,"2","0"
the third string contains special term wether it is inititalized or declared : 
"i" : initilized - The variable moves in this case
"f" : - The variable has a value in its place and the value will be moved in semantic rules .
The memory array is initiatlly initiated by ("%","%","%") of required size.

#### Task : 3
Signature and Structures of VMC machine
The signature and Structure of VMC machine is defined in VMC.sml file.
In this file there are all other helper functions which are used to create a structure Vmc.

#### Task : 4
There is a function called posfix which simply gives the postfix of a tree node (not entire tree in postfix form)
The postfix function happens only when required.

#### Task : 5

There is a function execute() and initiate().
### initiate() :- 
This function takes the input of DS ( declarative statements in the AST formed ) and Memory array.
and initiates all the variables,
Allocates the space for every variable given.
Firstly,We should give the DS part of AST to this fuction 

### execute() :-
This function taked the input of CS ( command statements in the AST formed ) and executes the VMC state from initial state to final sate based on the semmantic rules given.

#### Sample Programs and their outputs from functions :
1.)
program Myprog :: 
	var a,b,c : int ;{ 
	 a := 3 + 2;
	 b := 1;
}
output :
inititaly from initialize() :
val it =
  [|("c","%","i"),("b","%","i"),("a","%","i"),("%","%","%"),("%","%","%"), ("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%")|]: (string * string * string) array
  
initial VMC state :
([],M,[NODE("SEQ",NODE ("SET",LEAF "a",NODE ("PLUS",LEAF "3",LEAF "2")),NODE ("SEQ",NODE ("SET",LEAF "b",LEAF "1"),NULL))])
final VMC state :
after passing through execute() :-
([],[|("c","%","i"),("b","1","f"),("a","5","f"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%")|],[])


2.)
program Myprog :: 
	var a,b,c : int ;{ 
	 a := 3 + 2;
	 b := 1;
	 a := a+1;
}
output :
inititaly from initialize() :
val it =
  [|("c","%","i"),("b","%","i"),("a","%","i"),("%","%","%"),("%","%","%"), ("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%")|]: (string * string * string) array
  
initial VMC state :
([],M,[NODE ("SEQ",NODE ("SET",LEAF "a",NODE ("PLUS",LEAF "3",LEAF "2")),NODE("SEQ",NODE ("SET",LEAF "b",LEAF "1"),NODE("SEQ",NODE ("SET",LEAF "a",NODE ("PLUS",LEAF "a",LEAF "1")),NULL)))])
final VMC state :
after passing through execute() :-
([],[|("c","%","i"),("b","1","f"),("a","6","f"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%")|],[])

3.)
program Myprog :: 
	var a,b,c : int ;{ 
	 a := 3 + 2;
	 b := 1;
	 a := a-1;
}
output :
inititaly from initialize() :
val it =
  [|("c","%","i"),("b","%","i"),("a","%","i"),("%","%","%"),("%","%","%"), ("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%")|]: (string * string * string) array
  
initial VMC state :
([],M,[NODE("SEQ",NODE ("SET",LEAF "a",NODE ("PLUS",LEAF "3",LEAF "2"))NODE("SEQ",NODE ("SET",LEAF "b",LEAF "1"),NODE("SEQ",NODE ("SET",LEAF "a",NODE ("MINUS",LEAF "a",LEAF "1")),NULL)))])
final VMC state :
after passing through execute() :-
([],[|("c","%","i"),("b","1","f"),("a","4","f"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%")|],[])

4.)
program Myprog :: 
	var a,b : int ;{ 
	   a := 3 +2;
	   b := 1;
    if( a > b) then{ 
	 	a := 3;
	 } else {
	 	a := 2;
	 }
	 endif;
}
output :
inititaly from initialize() :
val it =
  [|("c","%","i"),("b","%","i"),("a","%","i"),("%","%","%"),("%","%","%"), ("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%")|]: (string * string * string) array
  
initial VMC state :
([],M,[NODE("SEQ",NODE ("SET",LEAF "a",NODE ("PLUS",LEAF "3",LEAF "2")),NODE("SEQ",NODE ("SET",LEAF "b",LEAF "1"),NODE("SEQ",NODE("ITE",NODE ("GT",LEAF "a",LEAF "b"),NODE("IF CS",NODE ("SEQ",NODE ("SET",LEAF "a",LEAF "3"),NULL),NODE ("SEQ",NODE ("SET",LEAF "a",LEAF "2"),NULL))),NULL)))])
final VMC state :
after passing through execute() :-
([],[|("c","%","i"),("b","1","f"),("a","3","f"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%")|],[])

5.)
program Myprog :: 
	var a,b : int ;{ 
	   a := 3 +2;
	   b := 1;
	   while(a > 0 ) do {
	   	a := a -1;
	   }endwh;	
}
output :
inititaly from initialize() :
val it =
  [|("c","%","i"),("b","%","i"),("a","%","i"),("%","%","%"),("%","%","%"), ("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%")|]: (string * string * string) array
  
initial VMC state :
([],M,[NODE("SEQ",NODE ("SET",LEAF "a",NODE ("PLUS",LEAF "3",LEAF "2")),NODE("SEQ",NODE ("SET",LEAF "b",LEAF "1"),NODE("SEQ",NODE("WH",NODE ("GT",LEAF "a",LEAF "0"),NODE("DO",NODE("SEQ",NODE("SET",LEAF "a",NODE ("MINUS",LEAF "a",LEAF "1")),NULL),NULL)),NULL)))])
final VMC state :
after passing through execute() :-
([], [|("c","%","i"),("b","1","f"),("a","0","f"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%"),("%","%","%")|],[])

#### Acknowledgements :
Took reference of ML-lex, ML-yacc , glue code function from :
http://rogerprice.org/ug/ug.pdf
https://www.cs.princeton.edu/~appel/modern/ml/ml-lex/manual.html
http://www.smlnj.org/doc/ML-Yacc/
Modern Compiler Implementation in ML ( CPENTalk.com ).pdf
