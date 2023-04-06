# AST-WHILE
AST-WHILE
# Assignment - 3
###### submission 2020CS10356  (Madaka Jaya Prakash)
## AST for WHILE language in SML
##### Files in the directory :
1. while.lex
2. while.yacc
3. while_ast.sml
4. loader.sml
5. program.txt
##### Running Commands :
ml-lex while.lex
ml-yacc while.yacc
sml loader.sml
##### Files generated :
1. while.lex.sml
2. while.yacc.desc
3. while.yacc.sig
4. while.yacc.sml


To increase print depth print Control.Print.printDepth := 100;
##### overview of assignment :
The program code is written in program.txt file which should be written in WHILE language in pg : 202 of Hyper Notes.
The program is in EBNF language. The symmantic rules and Grammer are explained in the file later.
lexical errors and parsing errors are thrown when required.
The SML program prints CalcParse.result value which is an AST of the given code in program.txt.
A data type tree is defined in this SML code , which is a binary tree which has node values as strings.
### Symmantic Rules of the Context-free grammer used :
##### terminal symbols used are :
"program" , "var" , "int" , "bool" , "::" , ":" , ";", "{" , "}" , ":=" , "if" , "then" , "else" , "endif" , "while" , "do" , "endwh" , "(" , ")" , "~" , "+" , "-" , "*" , "/" , "<" , “<=” , “=” , “>” , “>=” , “<>” , "%" , "tt" , "ff" , "&&" , "||" , "!"  all the chars and their combinations along with digits anf all the digits are terminal symbols in this language.
These Terminal Sumbols are given some Token names in while.lex files which are used in parsing in while.yacc.
##### Non terminal symbols used are :
 START , E  , DS , CS , D , VL , VLE , CO , C , EXP , IEXP , BEXP , IT , IFAC , NUME , BT , BF , COMP .
##### Rules :
Grammer and symmantic rules
ID is Token for respective name (string). look while.lex for Token  ID .
lookup is a function which converts tokens to string in while.yacc
| Context-free Grammer     | Symmantic Rule |
| ----------- | ----------- |
| START  &rarr; E      | SOME(E)      |
| E &rarr; "program" ID "::" DS CS  |   NODE("PROG" , DS, CS)    |
| DS &rarr; D DS | NODE("DS",D,DS) |
| DS &rarr; (epsilon) | LEAF(".") |
| D &rarr; "var" VL ":" "int" ";" 	|NODE("VAR",LEAF("INT"),VL)|
|D &rarr; "var" VL ":" "bool" ";" 	|NODE("VAR",LEAF("BOOL"),VL)|
|VL &rarr; ID VLE 					|	NODE(lookup ID,LEAF("."),VLE) |
|VLE &rarr; "," ID VLE 				|	NODE(lookup ID,LEAF("."),VLE) |
|  VLE &rarr; (epsilon)						|	LEAF(".")|
|CS &rarr; "{" "}"			|LEAF(".")|
|CS &rarr; "{" C ";" CO "}"			|	NODE("CS",C,CO)|
|CO &rarr; C  ";"  CO 				|	NODE("CS",C,CO)|
|CO &rarr; (epsilon)    |   LEAF(".")|
|C &rarr; ID ":=" EXP 				|	NODE("EQ",LEAF(lookup ID),EXP)|
|C &rarr; "read" ID 				|		NODE("READ",LEAF(lookup ID) , LEAF("."))|
|C &rarr; "write" IEXP 				|	NODE("WRITE",IEXP,LEAF(".")) |
|C &rarr; "if" BEXP "then" CS "else" CS "endif" 	|		NODE("IF",BEXP,NODE("IF CMD",CS1,CS2))|
|C &rarr; "while" BEXP "do" CS "endwh"		|		NODE("WHILE",BEXP,NODE("DO",CS,LEAF("."))) |
|EXP &rarr; IEXP						|IEXP|
|EXP &rarr; BEXP					|	BEXP|
|IEXP &rarr; IEXP "+" IT				|	NODE("PLUS",IEXP,IT)|
|IEXP &rarr; IEXP MINUS IT				|	NODE("MINUS",IEXP,IT)|
|IEXP &rarr; IT					|		IT|
|IT &rarr; IT "*" IFAC	    	|			NODE("TIMES",IT,IFAC)|
|IT &rarr; IT "/" IFAC		|			NODE("DIV",IT,IFAC)|
|IT &rarr; IT "%" IFAC		|			NODE("MOD",IT,IFAC)|
|IT &rarr; IFAC				|		IFAC|
|IFAC &rarr; NUME				|		NUME|
|IFAC &rarr; ID				|			LEAF(lookup ID)|
|IFAC &rarr; "(" IEXP ")"	|  				IEXP|
|IFAC &rarr; "~" IFAC			|			NODE("~",IFAC,LEAF("."))|
|BEXP &rarr; BEXP "\|\|" BT				|	NODE("OR",BEXP,BT|
|BEXP &rarr; BT				|			BT|
|BT &rarr; BT "&&" BF				|	NODE("AND",BT,BF)|
| BT &rarr; BF						|	BF|
|BF &rarr; "tt"					|	LEAF("TT")|
|BF &rarr; "ff"					|		LEAF("FF")|
|BF &rarr;  ID					|		LEAF(lookup ID)|
|BF &rarr; COMP				|		COMP|
|BF &rarr; "(" BEXP ")"			|		BEXP|
|BF &rarr; "!" BF				|		NODE("NOT",BF,LEAF("."))|
| COMP &rarr; IEXP "<" IEXP			|		NODE("LT",IEXP1,IEXP2)|
|COMP &rarr; IEXP "<=" IEXP			|		NODE("LEQ",IEXP1,IEXP2)|
|COMP &rarr; IEXP ">" IEXP			|		NODE("GT",IEXP1,IEXP2)|
|COMP &rarr; IEXP ">=" IEXP			|		NODE("GEQ",IEXP1,IEXP2)|
|COMP &rarr; IEXP "<>" IEXP			|		NODE("NEQ",IEXP1,IEXP2)|
|NUME &rarr; "+" NUM					|LEAF(Int.toString(NUM))|
|NUME &rarr; NUM					|	LEAF(Int.toString(NUM))|
|NUME &rarr; "-" NUM				|		LEAF("~"^Int.toString(NUM))|
#### Synatx Direxted Translation (Symmantic Rules )
Symmantic ryles are in the right colon of above table.
Functions used are NODE() , LEAF() :
 1. NODE() function creates a binary tree node whose node.val is a string and contains two other trees as its child
 2. LEAF() function creates a leaf node whose value is a string
 
This data type is created in SML file in while.yacc user-defnitions .

#### Data Types Used :
Tree data Type is used to make AST.
Tokens is a data type used in ML-lex(while.lex) file.
this tokens data type carries the line and coloumn number of every string every string is made into a token.
There are specific tokens such as "program" : PROG and "var" : var ,......
remaing all alphabets are considerd as ID Token which can be used in lookup() function.

#### Auxillary Functions :
1. lookup()  :- all the different forms of string goes to ID token and converted into string by lookup()
2. CalcLexFun() :- lexer function with this lexewr can be used 
3. invokelexstream() :- prints errors in lexing eg (unknown entry symbol).
4. stringToLexer() :- converts input string program to lexing tokens.
5. parse() :- send Tokens to parser made in ML-yacc.
6. AST_parser() :- makes AST tree while parsing the string.


#### Acknowledgements :
Took reference of ML-lex, ML-yacc , glue code function from :
http://rogerprice.org/ug/ug.pdf
https://www.cs.princeton.edu/~appel/modern/ml/ml-lex/manual.html
http://www.smlnj.org/doc/ML-Yacc/
Modern Compiler Implementation in ML ( CPENTalk.com ).pdf
