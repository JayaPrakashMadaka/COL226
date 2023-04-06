fun getelement(l,0)=hd(l)
  | getelement(l,k)=getelement(tl(l),k-1)

	(* function to get element from a list to use it as an array*)

fun Bitor(0,0)=0
  | Bitor(1,_)=1
  | Bitor(_,1)=1
  | Bitor(x,y)=0;

	(*function which computes or operation on two bits*) 

fun Bitand(0,_)=0
  | Bitand(_,0)=0
  | Bitand(x,y)=1;

	(*function which computes and operation on two bits*)

fun findt((a,b,c,d),0)=a
  | findt((a,b,c,d),1)=b
  | findt((a,b,c,d),2)=c
  | findt((a,b,c,d),3)=d
  | findt((a,b,c,d),_)=0;

	(*function to get element in Quadraple*)

fun getLine() = (
print "Please enter a integer as input : ";
let
    val i : int = valOf (Int.fromString (valOf (TextIO.inputLine TextIO.stdIn)))
    in
    i
end
);

	(*function to get input which automatically converts string to int written by using Standard Library of SML *)
 
fun op1(l,k,v : int)=if k=0 then v::tl(l) else hd(l)::op1(tl(l),k-1,v);
fun op2(l,i,k)=let val x  = getelement(l,i) in op1(l,k,x) end;
fun op3(l,i,k)=let val x=getelement(l,i) in if x=0 then op1(l,k,1) else op1(l,k,0) end;
fun op4(l,i,j,k)=let val x=Bitor(getelement(l,i),getelement(l,j)) in op1(l,k,x) end;
fun op5(l,i,j,k)=let val x=Bitand(getelement(l,i),getelement(l,j)) in op1(l,k,x) end;
fun op6(l,i,j,k)=let val x=getelement(l,i)+getelement(l,j) in op1(l,k,x) end;
fun op7(l,i,j,k)=let val x=getelement(l,i)-getelement(l,j) in op1(l,k,x) end;
fun op8(l,i,j,k)=let val x=getelement(l,i)*getelement(l,j) in op1(l,k,x) end;
fun op9(l,i,j,k)=let val x=getelement(l,i) div getelement(l,j) in op1(l,k,x) end;
fun op10(l,i,j,k)=let val x=getelement(l,i) mod getelement(l,j) in op1(l,k,x) end;
fun op11(l,i,j,k)=if getelement(l,i)=getelement(l,j) then op1(l,k,1) else op1(l,k,0);
fun op12(l,i,j,k)=if getelement(l,i)>getelement(l,j) then op1(l,k,1) else op1(l,k,0);

	(* opcodes which perform respective operations*)

fun f((1,_,_,k),mem)=let val x= getLine() in op1(mem,k,x) end
  | f((2,i,_,k),mem)=op2(mem,i,k)
  | f((3,i,_,k),mem)=op3(mem,i,k)
  | f((4,i,j,k),mem)=op4(mem,i,j,k)
  | f((5,i,j,k),mem)=op5(mem,i,j,k)
  | f((6,i,j,k),mem)=op6(mem,i,j,k)
  | f((7,i,j,k),mem)=op7(mem,i,j,k)
  | f((8,i,j,k),mem)=op8(mem,i,j,k)
  | f((9,i,j,k),mem)=op9(mem,i,j,k)
  | f((10,i,j,k),mem)=op10(mem,i,j,k)
  | f((11,i,j,k),mem)=op11(mem,i,j,k)
  | f((12,i,j,k),mem)=op12(mem,i,j,k)
  | f((16,i,_,k),mem)=op1(mem,k,i)
  | f((_,_,_,_),mem)=[];

	(*function f used to apply opcode on given mem array/list *)
 
 fun interpreter(A,mem,i,f)=
      let val l=getelement(A,i) in
      let val x=findt(l,0) in 
      let val y=findt(l,1) in
      let val z=findt(l,3) in 
 			if x=0 then print("PROGRAM HALTED"^"\n")
 			else if x=15 then print("The answer is : "^Int.toString(getelement(mem,y))^"\n")
 			else if (x=13 andalso getelement(mem,y)=1) then interpreter(A,mem,z,f)
      else if (x=13 andalso getelement(mem,y)=0) then interpreter(A,mem,i+1,f)
 			else if x=14 then interpreter(A,mem,z,f)
 			else interpreter(A,f(getelement(A,i),mem),i+1,f)
      end
    end
  end
      end;


fun bdiminterpreter(A,mem)=interpreter(A,mem,0,f);


	(*BDIM INTERPRETER which runs from input file and changes corresponding mem elements*)


	(*File IO done here *)


fun readLine (file : string) = let 
  val x = TextIO.openIn file 
  fun loop x = 
   case TextIO.inputLine x of 
      SOME line =>  line :: loop x 
    | NONE      => [] 
   in 
    loop x before TextIO.closeIn x 
   end ;


fun read(i,j,s,l)= if i=size(s)-1 then l 
  		else if (substring(s,j,1)="," orelse substring(s,j,1)=")") then read(j+1,j+1,s,l@[valOf(Int.fromString(substring(s,i,j-i)))])
		  else read(i,j+1,s,l);

fun QuadfromList([a,b,c,d])=(a,b,c,d)
  | QuadfromList(l)=(0,0,0,0);

fun R(s)=QuadfromList(read(1,1,s,[]));

fun Myfunc([])=[]
  | Myfunc(l)=R(hd(l))::Myfunc(tl(l));

		(*maxMemSize < 8 for all functions given *)
		
		(*Function Interpreter*)

fun interpret(file)=bdiminterpreter(Myfunc(readLine(file)),[0,0,0,0,0,0,0,0,0,0]);
