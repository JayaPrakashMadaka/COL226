datatype tree = NODE of string * tree * tree 				(* AST datatype done in  binary tree *)
		| LEAF of string;

structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)					(*from parser *)
structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens);					
structure CalcParser =												(* gluing lex and par *)
	  Join(structure LrParser = LrParser
     	       structure ParserData = CalcLrVals.ParserData
     	       structure Lex = CalcLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")	(* parse  error finder *)
		in
		    CalcParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end  	
		
fun parse (lexer) =
    let val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer									(*parser*)
	val (nextToken, lexer) = CalcParser.Stream.get lexer
    in
        if CalcParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val AST_parser = parse o stringToLexer									(* AST conversion while parsing *)


fun readLine (file : string) = let 
  val x = TextIO.openIn file 
  fun loop x = 
   case TextIO.inputLine x of 										(* text input from file converting to string and sent to parser *)
      SOME line =>  line ^ loop x 
    | NONE      => ""
   in 
    loop x before TextIO.closeIn x 
   end ;

readLine("program.txt");											(* input file "taken program.txt" *)

Control.Print.printDepth := 100;

AST_parser(readLine("program.txt"));
