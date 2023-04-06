exception EmptyList;
fun remove(x::xs,0) = x::xs
  | remove(x::xs,i) = remove(xs,i-1)
  | remove([],_) = raise EmptyList;
fun findl(f,x::xs) = if f(x) = true then x else findl(f,xs)
  | findl(f,[]) = raise EmptyList;
fun filterl(f,x::xs) = if f(x) = true then x::filterl(f,xs) else filterl(f,xs)
  | filterl(f,[])=[];
fun alll(f,x::xs) = if f(x) = false then false else alll(f,xs)
  | alll(f,[]) = true;
fun apply(f,x::xs) = f(x) :: apply(f,xs)
   | apply(f,[]) = [];
fun get(x::xs,0) = x
  | get(x::xs,i)=get(xs,i-1)
  | get([],_) = raise EmptyList;
datatype Stack = Stack of int list
          exception EmptyStack;
fun stack2list(Stack(x::xs)) = x::xs
  | stack2list(Stack([])) = [];
fun list2stack(x::xs)=Stack(x::xs)
  | list2stack([])=Stack([]);
val create = Stack([]);
fun push(S : Stack , a : int ) = if S = Stack([]) then Stack([a]) else let val L=stack2list(S) in Stack(a::L) end;
fun  pop(Stack([])) = raise EmptyStack
  | pop(Stack([_])) = Stack([])
  | pop(Stack(_::xs)) = Stack(xs);
fun top(Stack([])) = raise EmptyStack
  | top(Stack(x::xs)) = x;
fun empty(S) = if S = Stack([]) then true else false;
fun poptop(Stack([])) = raise EmptyStack
  | poptop(Stack(x::xs)) = (x,Stack(xs));
fun nth(S,i) = let val L = stack2list(S) in get(L,i) end;
fun drop(S,i) = let val L = stack2list(S) in Stack(remove(L,i)) end;
fun depth(S) = let val L = stack2list(S) in length(L) end;
fun map(f , S) = let val L = stack2list(S) in list2stack(apply(f, L)) end;
fun find(f,S) = let val L = stack2list(S) in findl(f,L) end;
fun filter(f,S) = let val L = stack2list(S) in filterl(f,L) end;
fun all(f,S) = let val L = stack2list(S) in alll(f,L) end;
