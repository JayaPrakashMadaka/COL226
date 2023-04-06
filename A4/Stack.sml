fun mapl(f,x::xs) = f(x) :: mapl(f,xs)
 |  mapl(f,[]) = [];
fun SL(f,x::xs) =f(x)^","^SL(f,xs)
 |  SL(f,[])="";
signature STACK =
sig
	type 'a stack
	exception EmptyStack
	val create: 'a stack
	val push : 'a * 'a stack -> 'a stack
	val pop : 'a stack -> 'a stack
	val top : 'a stack -> 'a
	val empty: 'a stack -> bool
	val poptop : 'a stack -> 'a * 'a stack
	val nth : 'a stack * int -> 'a
	val drop : 'a stack * int -> 'a stack
	val depth : 'a stack -> int
	val map : ('a -> 'b) -> 'a stack -> 'b stack
	val app : ('a -> unit) -> 'a stack -> unit 	
	val mapPartial : ('a -> 'b option) -> 'a stack -> 'b stack
	val find : ('a -> bool) -> 'a stack -> 'a option
	val filter : ('a -> bool) -> 'a stack -> 'a stack
	val foldr : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b
	val foldl : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b
	val exists : ('a -> bool) -> 'a stack -> bool
	val all : ('a -> bool) -> 'a stack -> bool
	val list2stack : 'a list -> 'a stack (* Convert a list into a stack *)
	val stack2list: 'a stack -> 'a list (* Convert a stack into a list *)
	val toString: ('a -> string) -> 'a stack -> string
end;
structure FunStack : STACK =
struct 
    exception EmptyStack;
    type 'a stack = 'a list;
    val create = [];
    fun push(a,S) = a::S;
    fun pop(x::xs) = xs
     |	pop([]) = raise EmptyStack;
    fun top(x::xs) = x
     |  top([]) = raise EmptyStack;
    fun empty(L) = if length(L) = 0 then true else false;
    fun poptop([]) = raise EmptyStack
     |  poptop([x]) = (x,[])
     |	poptop(x::xs) = (x,xs);
    fun nth(x::xs,0) = x
     | nth(x::xs,i)=nth(xs,i-1)
     | nth([],_) = raise EmptyStack;
    fun drop(x::xs,0) = x::xs
     | drop(x::xs,i) = drop(xs,i-1)
     | drop([],_) = raise EmptyStack;
    fun depth(S) = length(S);
    val map = fn f => fn l => List.map f l;
    val app = fn f => fn l => List.app f l;
    val mapPartial = fn f => fn l=> List.mapPartial f l;
    val find = fn f => fn l => List.find f l;
    val filter = fn f => fn l => List.filter f l;
    val foldr = fn f => fn b => fn S => List.foldr f b S;
    val foldl = fn f => fn b => fn S => List.foldl f b S;
    val exists = fn f => fn l => List.exists f l;
    val all = fn f => fn l => List.all f l;
    fun list2stack(x::xs) = push(x,list2stack(xs))
      | list2stack([]) = create;
    fun stack2list(x::xs)=x::stack2list(xs)
      | stack2list(create)=[];
    val toString = fn f => fn S => SL(f,S);
   
end;	
