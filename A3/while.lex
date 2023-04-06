structure Tokens= Tokens
  
  type pos = int								(* position of string taken *)
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 1								(* starting line of code and then increments for error generation*)
  val eof = fn () => Tokens.EOF(!pos, !pos)					(* end of file *)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")	
										(* error function for lexer gives line and error *)
  
%%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));			

alpha=[A-Za-z0-9];
digit=[0-9];
ws=[\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{digit}+ => (Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !pos, !pos));
"+"      => (Tokens.PLUS(!pos,!pos));
"*"      => (Tokens.TIMES(!pos,!pos));
"{"      => (Tokens.LCPAREN(!pos,!pos));
"}"      => (Tokens.RCPAREN(!pos,!pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
"program"  => (Tokens.PROG(!pos,!pos));
"::"	 => (Tokens.BLK(!pos,!pos));
"var"	 => (Tokens.VAR(!pos,!pos));
"int" 	 => (Tokens.INT(!pos,!pos));
"bool"	 => (Tokens.BOOL(!pos,!pos));
"read"	 => (Tokens.READ(!pos,!pos));
"write"  => (Tokens.WRITE(!pos,!pos));
":"	 => (Tokens.TYPE(!pos,!pos));
";"	 => (Tokens.EOC(!pos,!pos));
","	 => (Tokens.COM(!pos,!pos));
":="	 => (Tokens.EQ (!pos,!pos));
">"	 => (Tokens.GT(!pos,!pos));
"<"	 => (Tokens.LT(!pos,!pos));
"<="	 => (Tokens.LEQ(!pos,!pos));
">="	 => (Tokens.GEQ(!pos,!pos));
"<>"	 => (Tokens.NEQ(!pos,!pos));
"%"	 => (Tokens.MOD(!pos,!pos));
"if"	 => (Tokens.IF(!pos,!pos));
"then"	 => (Tokens.THEN(!pos,!pos));
"else"	 => (Tokens.ELSE(!pos,!pos));
"endif"  => (Tokens.ENDIF(!pos,!pos));
"while"  => (Tokens.WHILE(!pos,!pos));
"do"	 => (Tokens.DO(!pos,!pos));
"endwh"  => (Tokens.ENDWH(!pos,!pos));
"&&"	 => (Tokens.AND(!pos,!pos));
"||"	 => (Tokens.OR(!pos,!pos));
"tt"	 => (Tokens.TT(!pos,!pos));
"ff"	 => (Tokens.FF(!pos,!pos));
"~"	 => (Tokens.NEG(!pos,!pos));
"!"	 => (Tokens.NOT(!pos,!pos));
{alpha}+ => (Tokens.ID(yytext,!pos,!pos));
"-"      => (Tokens.MINUS(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));
"."      => (error ("ignoring bad character "^yytext,!pos,!pos);lex());		
