use "Stack.sml";
exception NullpointException;
datatype tree = NODE of string * tree * tree
              | LEAF of string
              | NULL;
fun nodeval(LEAF(s))=s
  | nodeval(NODE(s,_,_)) = s
  | nodeval(NULL) = "NULL";
fun getleft(NODE(_,T,_))=T
  | getleft(LEAF(_)) = NULL
  | getleft(NULL) = raise NullpointException;
fun getright(NODE(_,_,T))=T
  | getright(LEAF(_)) = NULL
  | getright(NULL) = raise NullpointException;
fun makenode(s : string) = LEAF(s);
fun isLeaf(LEAF(_)) = true
 |  isLeaf(NODE(_,_,_)) = false
 |  isLeaf(NULL) = false;
fun get(a : int ,(x : string,y : string, z : string)) = if a=1 then x else if a = 2 then y else z;
fun getval1(x : string , M : (string * string * string ) array,i : int ) = if(get(1,Array.sub(M,i)) = x) then get(2,Array.sub(M,i)) else getval1(x,M,i+1);
fun getval(x : string , M : (string * string * string ) array ) = getval1(x,M,0);
fun opr("PLUS" , p : string ,q : string ) = Int.toString(valOf(Int.fromString(p)) + valOf(Int.fromString(q)))
 |  opr("MINUS",p,q) = Int.toString(valOf(Int.fromString(p))-valOf(Int.fromString(q)))
 |  opr("TIMES",p,q) = Int.toString(valOf(Int.fromString(p))*valOf(Int.fromString(q)))
 |  opr("DIV" , p,q) =Int.toString( valOf(Int.fromString(p)) div valOf(Int.fromString(q)))
 |  opr("MOD" ,p,q) = Int.toString(valOf(Int.fromString(p)) mod valOf(Int.fromString(q)))
 |  opr("OR" , "1",_) = "1"
 |  opr("OR" , _,"1") = "1"
 |  opr("OR" , "0","0") = "0"
 |  opr("AND","1","1") = "1"
 |  opr("AND","0",_) = "0"
 |  opr("AND",_,"0") = "0"
 |  opr("GT",p,q) = if (valOf(Int.fromString(p)) > valOf(Int.fromString(q))) then "1" else "0"
 |  opr("LT",p,q) = if (valOf(Int.fromString(p)) < valOf(Int.fromString(q))) then "1" else "0"
 |  opr("EQ",p,q) = if (valOf(Int.fromString(p)) = valOf(Int.fromString(q))) then "1" else "0"
 |  opr("NEQ",p,q) = if (valOf(Int.fromString(p)) <> valOf(Int.fromString(q))) then "1" else "0"
 |  opr("LEQ",p,q) = if( valOf(Int.fromString(p)) <= valOf(Int.fromString(q))) then "1" else "0"
 |  opr("GEQ",p,q) = if( valOf(Int.fromString(p)) >= valOf(Int.fromString(q))) then "1" else "0";

fun set1(M : (string * string * string ) array , x : string , m : string , i : int) = if (get(1,Array.sub(M,i)) = x) then Array.update(M,i,(x,m,"f")) else set1(M,x,m,i+1);


fun set(M : (string * string * string ) array , x : string , m : string) = set1(M,x,m,0);
signature VMC =
sig
	val Em : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val Ex : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val Eo00 : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val Eo01 : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val Eo10 : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val Eo11 : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val Eo : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val C : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val C0 :tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val C1 : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val C2 : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val Citeq : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val Cite0 : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val Cite1 : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val Cwhq : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val Cwh0 : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
	val Cwh1 : tree FunStack.stack * (string * string * string) array  * tree FunStack.stack -> tree FunStack.stack * (string * string * string) array * tree FunStack.stack
end;
structure Vmc : VMC =
struct
	fun Em(V,M,C) = let val x= FunStack.top(C) in (FunStack.push(x,V),M,FunStack.pop(C)) end;
	fun Ex(V,M,C) = let val x = makenode(getval(nodeval(FunStack.top(C)),M)) in  (FunStack.push(x,V),M,FunStack.pop(C)) end;
	fun Eo00(V,M,C) = let val x= nodeval(FunStack.nth(C,0)) in let val y = nodeval(FunStack.nth(C,1)) in 
				(FunStack.push(makenode(y),FunStack.push(makenode(x),V)),M,FunStack.pop(FunStack.pop(C))) end end;
	fun Eo01(V,M,C) = let val x = nodeval(FunStack.nth(C,0)) in let val y = getval(nodeval(FunStack.nth(C,1)),M) in
				(FunStack.push(makenode(y),FunStack.push(makenode(x),V)),M,FunStack.pop(FunStack.pop(C))) end end;
	fun Eo10(V,M,C) = let val x = getval(nodeval(FunStack.nth(C,0)),M) in let val y = nodeval(FunStack.nth(C,1)) in
				(FunStack.push(makenode(y),FunStack.push(makenode(x),V)),M,FunStack.pop(FunStack.pop(C))) end end;
	fun Eo11(V,M,C) = let val x = getval(nodeval(FunStack.nth(C,0)),M) in let val y = getval(nodeval(FunStack.nth(C,1)),M) in
				(FunStack.push(makenode(y),FunStack.push(makenode(x),V)),M,FunStack.pop(FunStack.pop(C))) end end;
	fun Eo(V,M,C) = let val x = opr(nodeval(FunStack.top(C)),nodeval(FunStack.nth(V,1)),nodeval(FunStack.nth(V,0))) in (FunStack.push(makenode(x),FunStack.pop(FunStack.pop(V))),M,FunStack.pop(C)) end;
	fun C(V,M,C) = (V,M,FunStack.pop(C));
	fun C0(V,M,C) = let val x= FunStack.top(C) in (FunStack.push(x,V),M,FunStack.pop(C)) end;
	fun C1(V,M,C) = let val m = nodeval(FunStack.top(V)) in let val x = nodeval(FunStack.nth(V,1)) in let val var = set(M,x,m) in (FunStack.pop(FunStack.pop(V)),M,FunStack.pop(C)) end end end;
	fun C2(V,M,C) = let val x = FunStack.nth(C,0) in let val y = FunStack.nth(C,1) in (V,M,FunStack.push(x,FunStack.push(y,FunStack.pop(FunStack.pop(FunStack.pop(C)))))) end end;
	fun Citeq(V,M,C) = (FunStack.push(FunStack.top(C),V),M,FunStack.pop(C));
	fun Cite0(V,M,C) = let val d = FunStack.nth(C,1) in (FunStack.pop(V),M,FunStack.push(d,FunStack.pop(FunStack.pop(FunStack.pop(C))))) end;
	fun Cite1(V,M,C) = let val c = FunStack.nth(C,0) in (FunStack.pop(V),M,FunStack.push(c,FunStack.pop(FunStack.pop(FunStack.pop(C))))) end; 
	fun Cwhq(V,M,C) = let val b = FunStack.nth(C,0) in let val c = FunStack.nth(C,1) in (FunStack.push(c,FunStack.push(b,V)),M,FunStack.push(b,FunStack.pop(FunStack.pop(C)))) end end;
	fun Cwh0(V,M,C) = (FunStack.pop(V),M,FunStack.pop(FunStack.pop(FunStack.pop(C))));
	fun Cwh1(V,M,C) = let val c = FunStack.nth(C,0) in let val b = FunStack.nth(C,1) in 
			(FunStack.pop(V),M,FunStack.push(c,FunStack.push(b,C))) end end;
end;



fun postfix(LEAF(S),L) = FunStack.push(LEAF(S), L)
 |  postfix(NODE(S,L,R),X) =  FunStack.push(L,FunStack.push(R,FunStack.push(LEAF(S),X)))
 |  postfix(NULL,L) = L;

Control.Print.printLength:=100;
Control.Print.printDepth:=100;
fun postini(LEAF(S),L) = FunStack.push(nodeval(LEAF(S)),L)
 |  postini(NODE(S,L,R),X) = postini(L,postini(R,FunStack.push(nodeval(NODE(S,L,R)),X)))
 |  postini(NULL,L) = L
fun giveM(M : (string * string * string ) array , x : string ,i ) = if ( get(1,Array.sub(M,i)) = "%" ) then
										Array.update(M,i,(x,"%","i")) else
											giveM(M,x,i+1); 
fun give(M : (string * string * string) array , x : string ) = giveM(M,x,0);
fun init(M : (string*string*string) array ,S : string FunStack.stack) =
							if (S = []) then
									M
							else if (FunStack.top(S) = "INT" orelse FunStack.top(S) = "VAR" orelse FunStack.top(S) = "BOOL" orelse FunStack.top(S) = "DS" )
									then	init(M,FunStack.pop(S))
							else 
								let val y = give(M,FunStack.top(S)) in init(M,FunStack.pop(S)) end;
fun initiate(T : tree , M : (string * string * string ) array) = init(M,postini(T,FunStack.create));



fun f(x : string , M : (string * string * string) array , i) = if (i = Array.length(M) ) then false
									else if (get(1,Array.sub(M,i)) = x andalso get(3,Array.sub(M,i))="f") then true
										else f(x,M,i+1);
fun search(x,M)=f(x,M,0);

fun g(x : string , M : (string * string * string) array , i) = if (i = Array.length(M) ) then false
									else if (get(1,Array.sub(M,i)) = x) then true
										else g(x,M,i+1);
fun searchnew(x,M)=g(x,M,0);

fun issymbol(s : string) = if( s= "SET" orelse s = "SEQ" orelse s = "PLUS" orelse s = "MINUS" orelse s = "TIMES" orelse s = "DIV" orelse s = "MOD" orelse s = "OR" orelse s = "AND" orelse s = "GT"
 orelse s = "LT"  orelse s = "GEQ"  orelse s = "LEQ"  orelse s = "IF CS"  orelse s = "NEQ" orelse s = "EQ" orelse s="ITE" orelse s ="WH" ) then true else false;
										
fun isbinop(T : tree) = if( isLeaf(T) = true andalso nodeval(T) = "PLUS" orelse nodeval(T) = "MINUS" orelse nodeval(T) = "TIMES" orelse nodeval(T) = "DIV" orelse nodeval(T) = "MOD" orelse
				nodeval(T) = "OR" orelse nodeval(T) = "AND" orelse nodeval(T) = "GT" orelse nodeval(T) = "LT" orelse nodeval(T) = "GEQ" orelse nodeval(T) = "LEQ" orelse nodeval(T) = "EQ" orelse nodeval(T) = "NEQ" ) then true
			else false; 







fun execute((V : tree FunStack.stack ,M : (string * string * string) array, C : tree FunStack.stack)) = 	if (FunStack.empty(C) = true ) then
										(V,M,C)
										else if (isLeaf(FunStack.nth(C,0)) = true andalso nodeval(FunStack.nth(C,0)) = "SEQ" ) then
											execute((V,M,FunStack.pop(C)))
										else if( isLeaf(FunStack.top(C)) = false andalso nodeval(FunStack.top(C)) = "ITE" ) then
											let val x = LEAF("ITE") in let val b = getleft(FunStack.top(C)) in let val c = getleft(getright(FunStack.top(C))) in let val d = getright(getright((FunStack.top(C)))) in execute((V,M,FunStack.push(b,FunStack.push(c,FunStack.push(d,FunStack.push(x,FunStack.pop(C))))))) end end end end
										else if( isLeaf(FunStack.top(C)) = false andalso nodeval(FunStack.top(C)) = "WH") then
											let val x = LEAF("WH") in let val b = getleft(FunStack.top(C)) in let val c = getleft(getright(FunStack.top(C))) in	execute((V,M,FunStack.push(b,FunStack.push(c,FunStack.push(b,FunStack.push(x,FunStack.pop(C))))))) end end end
										else if (FunStack.empty(C) = false andalso isLeaf(FunStack.top(C)) = false ) then
											 if(FunStack.depth(C) > 2) then
												if (isLeaf(FunStack.nth(C,2)) = true andalso nodeval(FunStack.nth(C,2)) = "ITE" andalso nodeval(FunStack.top(V)) = "1" )then
													execute(Vmc.Cite1(V,M,C))
												else if (isLeaf(FunStack.nth(C,2)) = true andalso nodeval(FunStack.nth(C,2)) = "ITE" andalso nodeval(FunStack.top(V)) = "0" )then
													execute(Vmc.Cite0(V,M,C))
												else if (isLeaf(FunStack.nth(C,2)) = true andalso nodeval(FunStack.nth(C,2)) = "WH" andalso nodeval(FunStack.top(V)) = "1") then
													execute(Vmc.Cwh1(V,M,C))
												else if (isLeaf(FunStack.nth(C,2)) = true andalso nodeval(FunStack.nth(C,2)) = "WH" andalso nodeval(FunStack.top(V)) = "0") then
													execute(Vmc.Cwh0(V,M,C))
							 					else
							 						execute((V,M,postfix(FunStack.top(C),FunStack.pop(C))))
							 				else
							 					execute((V,M,postfix(FunStack.top(C),FunStack.pop(C))))
										else if (FunStack.depth(C) >1) then
											if(isLeaf(FunStack.top(C)) = true andalso  searchnew(nodeval(FunStack.top(C)),M) = false andalso 
issymbol(nodeval(FunStack.top(C))) = false) then
												execute(Vmc.Em(V,M,C))
											else if(isLeaf(FunStack.top(C)) = true andalso search(nodeval(FunStack.top(C)),M) = false andalso  searchnew(nodeval(FunStack.top(C)),M) = true  andalso issymbol(nodeval(FunStack.top(C))) = false ) then
												execute(Vmc.Em(V,M,C))
											else if(isbinop(FunStack.top(C)) = true) then
												execute(Vmc.Eo(V,M,C)) 
											else if(isLeaf(FunStack.top(C)) = true andalso nodeval(FunStack.top(C)) = "SET") then
												execute(Vmc.C1(V,M,C))
											else if(isLeaf(FunStack.top(C)) = true andalso search(nodeval(FunStack.top(C)),M) = true  andalso issymbol(nodeval(FunStack.top(C))) = false andalso nodeval(FunStack.nth(C,2)) <> "SET" andalso nodeval(FunStack.nth(C,2)) <> "ITE") then
												execute(Vmc.Ex(V,M,C)) 
											else if(isLeaf(FunStack.top(C)) = true andalso isLeaf(FunStack.nth(C,2)) = true andalso nodeval(FunStack.nth(C,2)) = "SET" andalso searchnew(nodeval(FunStack.top(C)),M) = true) then
												execute(Vmc.C0(V,M,C)) 
											
											else 
												execute((V,M,FunStack.pop(C))) 	
										else 
											(V,M,C);


val M = Array.array(10,("%","%","%"));



initiate(NODE
          ("DS",
           NODE
             ("VAR",LEAF "INT",
              NODE ("a",NULL,NODE ("b",NULL,NODE ("c",NULL,NULL)))),NULL),M);


execute(([],M,[NODE
          ("SEQ",NODE ("SET",LEAF "a",NODE ("PLUS",LEAF "3",LEAF "2")),
           NODE
             ("SEQ",NODE ("SET",LEAF "b",LEAF "1"),
              NODE
                ("SEQ",
                 NODE
                   ("WH",NODE ("GT",LEAF "a",LEAF "0"),
                    NODE
                      ("DO",
                       NODE
                         ("SEQ",
                          NODE
                            ("SET",LEAF "a",NODE ("MINUS",LEAF "a",LEAF "1")),
                          NULL),NULL)),NULL)))]));







