(* User  declarations *)
datatype tree = NODE of string * tree * tree 				(* AST datatype done in  binary tree *)
		| LEAF of string;

fun lookup(s) = s;


%%
(* required declarations *)
%name Calc

%term
  ID of string | NUM of int
  | PLUS | TIMES | LCPAREN | RCPAREN | LPAREN | RPAREN | PROG | BLK | VAR | TYPE | EOC | EQ | GT | LT | GEQ | LEQ | NEQ | MOD | IF | THEN | ELSE | ENDIF | WHILE | DO | ENDWH  | EOF
  | AND | OR | TT | FF | NEG | NOT | MINUS | DIV | INT | BOOL | READ | WRITE | COM

%nonterm E of tree | START of tree option  | DS of tree | CS of tree | D of tree | VL of tree | VLE of tree | CO of tree | C of tree | EXP of tree | IEXP of tree | BEXP of tree
	| IT of tree | IFAC of tree | NUME of tree | BT of tree | BF of tree | COMP of tree

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%start START

%verbose

%%
START: E 						(SOME E)
      |    						(NONE)

  E:  PROG ID BLK DS CS 				(NODE("PROG" , DS, CS))
  DS: D DS 						(NODE("DS",D,DS))
  |  							(LEAF("."))
  D : VAR VL TYPE INT EOC 				(NODE("VAR",LEAF("INT"),VL))
  | VAR VL TYPE BOOL EOC  				(NODE("VAR",LEAF("BOOL"),VL))
  VL : ID VLE 						(NODE(lookup ID,LEAF("."),VLE))
  VLE : COM ID VLE 					(NODE(lookup ID,LEAF("."),VLE))
  | 							(LEAF("."))
  CS : LCPAREN  RCPAREN 				(LEAF("."))
  | LCPAREN C EOC CO RCPAREN 				(NODE("CS",C,CO))
  CO : C EOC CO 					(NODE("CS",C,CO))
  | 							(LEAF("."))
  C : ID EQ EXP 					(NODE("EQ",LEAF(lookup ID),EXP))
  | READ ID 						(NODE("READ",LEAF(lookup ID) , LEAF(".")))
  | WRITE IEXP 					(NODE("WRITE",IEXP,LEAF(".")))
  | IF BEXP THEN CS ELSE CS ENDIF 			(NODE("IF",BEXP,NODE("IF CS",CS1,CS2)))
  | WHILE BEXP DO CS ENDWH				(NODE("WHILE",BEXP,NODE("DO",CS,LEAF("."))))
  EXP : IEXP						(IEXP)
  | BEXP						(BEXP)
  IEXP : IEXP PLUS IT					(NODE("PLUS",IEXP,IT))
  | IEXP MINUS IT					(NODE("MINUS",IEXP,IT))
  | IT							(IT)
  IT : IT TIMES IFAC					(NODE("TIMES",IT,IFAC))
  | IT DIV IFAC					(NODE("DIV",IT,IFAC))
  | IT MOD IFAC					(NODE("MOD",IT,IFAC))
  | IFAC						(IFAC)
  IFAC : NUME						(NUME)
  | ID							(LEAF(lookup ID))
  | LPAREN IEXP RPAREN					(IEXP)
  | NEG IFAC						(NODE("~",IFAC,LEAF(".")))
  BEXP : BEXP OR BT					(NODE("OR",BEXP,BT))
  | BT							(BT)
  BT : BT AND BF					(NODE("AND",BT,BF))
  | BF							(BF)
  BF : TT						(LEAF("TT"))
  | FF							(LEAF("FF"))
  | ID							(LEAF(lookup ID))
  | COMP						(COMP)
  | LPAREN BEXP RPAREN					(BEXP)
  | NOT BF						(NODE("NOT",BF,LEAF(".")))
  COMP : IEXP LT IEXP					(NODE("LT",IEXP1,IEXP2))
  | IEXP LEQ IEXP					(NODE("LEQ",IEXP1,IEXP2))
  | IEXP GT IEXP					(NODE("GT",IEXP1,IEXP2))
  | IEXP GEQ IEXP					(NODE("GEQ",IEXP1,IEXP2))
  | IEXP NEQ IEXP					(NODE("NEQ",IEXP1,IEXP2))
  NUME : PLUS NUM					(LEAF(Int.toString(NUM)))
  | NUM						(LEAF(Int.toString(NUM)))
  | MINUS NUM						(LEAF("~"^Int.toString(NUM)))
