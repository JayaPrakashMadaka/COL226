functor CalcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Calc_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* User  declarations *)
exception NullpointException;
datatype tree = NODE of string * tree * tree
              | LEAF of string
              | NULL;
fun nodeval(LEAF(s))=s
  | nodeval(NODE(s,_,_)) = s
  | nodeval(NULL) = raise NullpointException;
fun getleft(NODE(_,T,_))=T
  | getleft(LEAF(_)) = NULL
  | getleft(NULL) = raise NullpointException;
fun getright(NODE(_,_,T))=T
  | getright(LEAF(_)) = NULL
  | getright(NULL) = raise NullpointException;
fun makenode(s : string) = LEAF(s);

fun lookup(s) = s;



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\005\000\000\000\
\\001\000\001\000\014\000\000\000\
\\001\000\001\000\021\000\006\000\020\000\022\000\019\000\026\000\018\000\
\\040\000\017\000\041\000\016\000\000\000\
\\001\000\001\000\035\000\002\000\034\000\003\000\033\000\007\000\032\000\
\\034\000\031\000\036\000\030\000\000\000\
\\001\000\001\000\036\000\000\000\
\\001\000\001\000\046\000\002\000\034\000\003\000\033\000\007\000\045\000\
\\032\000\044\000\033\000\043\000\034\000\031\000\035\000\042\000\
\\036\000\030\000\000\000\
\\001\000\001\000\051\000\000\000\
\\001\000\002\000\059\000\000\000\
\\001\000\002\000\062\000\000\000\
\\001\000\003\000\136\000\004\000\136\000\008\000\136\000\013\000\136\000\
\\015\000\136\000\016\000\136\000\017\000\136\000\018\000\136\000\
\\019\000\136\000\020\000\136\000\021\000\136\000\023\000\145\000\
\\027\000\145\000\030\000\145\000\031\000\145\000\036\000\136\000\
\\037\000\136\000\000\000\
\\001\000\003\000\058\000\008\000\089\000\015\000\071\000\016\000\070\000\
\\017\000\069\000\018\000\068\000\019\000\067\000\020\000\066\000\
\\036\000\057\000\000\000\
\\001\000\003\000\058\000\008\000\089\000\036\000\057\000\000\000\
\\001\000\003\000\058\000\015\000\071\000\016\000\070\000\017\000\069\000\
\\018\000\068\000\019\000\067\000\020\000\066\000\036\000\057\000\000\000\
\\001\000\005\000\012\000\000\000\
\\001\000\006\000\083\000\000\000\
\\001\000\008\000\099\000\031\000\064\000\000\000\
\\001\000\010\000\006\000\000\000\
\\001\000\012\000\022\000\000\000\
\\001\000\013\000\025\000\000\000\
\\001\000\013\000\079\000\000\000\
\\001\000\013\000\080\000\000\000\
\\001\000\013\000\082\000\000\000\
\\001\000\014\000\048\000\000\000\
\\001\000\023\000\075\000\031\000\064\000\000\000\
\\001\000\024\000\103\000\000\000\
\\001\000\025\000\105\000\000\000\
\\001\000\027\000\065\000\031\000\064\000\000\000\
\\001\000\028\000\102\000\000\000\
\\001\000\029\000\000\000\000\000\
\\001\000\038\000\050\000\039\000\049\000\000\000\
\\107\000\000\000\
\\108\000\009\000\004\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\011\000\009\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\042\000\024\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\001\000\021\000\022\000\019\000\026\000\018\000\040\000\017\000\
\\041\000\016\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\003\000\058\000\036\000\057\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\003\000\058\000\015\000\071\000\016\000\070\000\017\000\069\000\
\\018\000\068\000\019\000\067\000\020\000\066\000\036\000\057\000\000\000\
\\127\000\031\000\064\000\000\000\
\\128\000\004\000\056\000\021\000\055\000\037\000\054\000\000\000\
\\129\000\004\000\056\000\021\000\055\000\037\000\054\000\000\000\
\\130\000\004\000\056\000\021\000\055\000\037\000\054\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\030\000\063\000\000\000\
\\140\000\030\000\063\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\003\000\058\000\036\000\057\000\000\000\
\\150\000\003\000\058\000\036\000\057\000\000\000\
\\151\000\003\000\058\000\036\000\057\000\000\000\
\\152\000\003\000\058\000\036\000\057\000\000\000\
\\153\000\003\000\058\000\036\000\057\000\000\000\
\\154\000\003\000\058\000\036\000\057\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\"
val actionRowNumbers =
"\031\000\030\000\000\000\016\000\
\\034\000\034\000\013\000\001\000\
\\033\000\032\000\002\000\017\000\
\\039\000\018\000\003\000\004\000\
\\005\000\005\000\040\000\022\000\
\\029\000\037\000\006\000\043\000\
\\058\000\057\000\053\000\046\000\
\\007\000\003\000\003\000\008\000\
\\078\000\059\000\045\000\068\000\
\\065\000\063\000\026\000\012\000\
\\005\000\067\000\066\000\005\000\
\\009\000\023\000\005\000\019\000\
\\020\000\039\000\021\000\014\000\
\\003\000\003\000\003\000\003\000\
\\003\000\079\000\061\000\011\000\
\\077\000\005\000\005\000\013\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\070\000\015\000\
\\010\000\013\000\050\000\049\000\
\\044\000\036\000\035\000\038\000\
\\043\000\041\000\055\000\056\000\
\\054\000\052\000\051\000\060\000\
\\064\000\062\000\027\000\075\000\
\\072\000\074\000\071\000\073\000\
\\076\000\069\000\024\000\042\000\
\\048\000\013\000\025\000\047\000\
\\028\000"
val gotoT =
"\
\\001\000\001\000\002\000\104\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\006\000\005\000\005\000\000\000\
\\003\000\008\000\005\000\005\000\000\000\
\\004\000\009\000\000\000\
\\006\000\011\000\000\000\
\\000\000\
\\000\000\
\\009\000\013\000\000\000\
\\000\000\
\\007\000\021\000\000\000\
\\000\000\
\\011\000\027\000\013\000\026\000\014\000\025\000\015\000\024\000\000\000\
\\000\000\
\\011\000\039\000\012\000\038\000\013\000\026\000\014\000\025\000\
\\015\000\024\000\016\000\037\000\017\000\036\000\018\000\035\000\000\000\
\\011\000\039\000\012\000\045\000\013\000\026\000\014\000\025\000\
\\015\000\024\000\016\000\037\000\017\000\036\000\018\000\035\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\051\000\009\000\050\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\058\000\015\000\024\000\000\000\
\\011\000\059\000\013\000\026\000\014\000\025\000\015\000\024\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\039\000\013\000\026\000\014\000\025\000\015\000\024\000\
\\017\000\070\000\018\000\035\000\000\000\
\\000\000\
\\000\000\
\\011\000\072\000\012\000\071\000\013\000\026\000\014\000\025\000\
\\015\000\024\000\016\000\037\000\017\000\036\000\018\000\035\000\000\000\
\\000\000\
\\000\000\
\\010\000\076\000\011\000\075\000\012\000\074\000\013\000\026\000\
\\014\000\025\000\015\000\024\000\016\000\037\000\017\000\036\000\
\\018\000\035\000\000\000\
\\000\000\
\\000\000\
\\007\000\079\000\000\000\
\\000\000\
\\000\000\
\\014\000\082\000\015\000\024\000\000\000\
\\014\000\083\000\015\000\024\000\000\000\
\\014\000\084\000\015\000\024\000\000\000\
\\013\000\085\000\014\000\025\000\015\000\024\000\000\000\
\\013\000\086\000\014\000\025\000\015\000\024\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\039\000\013\000\026\000\014\000\025\000\015\000\024\000\
\\017\000\088\000\018\000\035\000\000\000\
\\011\000\039\000\013\000\026\000\014\000\025\000\015\000\024\000\
\\016\000\089\000\017\000\036\000\018\000\035\000\000\000\
\\004\000\090\000\000\000\
\\011\000\091\000\013\000\026\000\014\000\025\000\015\000\024\000\000\000\
\\011\000\092\000\013\000\026\000\014\000\025\000\015\000\024\000\000\000\
\\011\000\093\000\013\000\026\000\014\000\025\000\015\000\024\000\000\000\
\\011\000\094\000\013\000\026\000\014\000\025\000\015\000\024\000\000\000\
\\011\000\095\000\013\000\026\000\014\000\025\000\015\000\024\000\000\000\
\\011\000\096\000\013\000\026\000\014\000\025\000\015\000\024\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\098\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\099\000\009\000\050\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\102\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 105
val numrules = 51
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | ID of unit ->  (string)
 | COMP of unit ->  (tree) | BF of unit ->  (tree)
 | BT of unit ->  (tree) | NUME of unit ->  (tree)
 | IFAC of unit ->  (tree) | IT of unit ->  (tree)
 | BEXP of unit ->  (tree) | IEXP of unit ->  (tree)
 | EXP of unit ->  (tree) | C of unit ->  (tree)
 | CO of unit ->  (tree) | VLE of unit ->  (tree)
 | VL of unit ->  (tree) | D of unit ->  (tree)
 | CS of unit ->  (tree) | DS of unit ->  (tree)
 | START of unit ->  (tree option) | E of unit ->  (tree)
end
type svalue = MlyValue.svalue
type result = tree option
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 28) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "PLUS"
  | (T 3) => "TIMES"
  | (T 4) => "LCPAREN"
  | (T 5) => "RCPAREN"
  | (T 6) => "LPAREN"
  | (T 7) => "RPAREN"
  | (T 8) => "PROG"
  | (T 9) => "BLK"
  | (T 10) => "VAR"
  | (T 11) => "TYPE"
  | (T 12) => "EOC"
  | (T 13) => "EQ"
  | (T 14) => "ISEQ"
  | (T 15) => "GT"
  | (T 16) => "LT"
  | (T 17) => "GEQ"
  | (T 18) => "LEQ"
  | (T 19) => "NEQ"
  | (T 20) => "MOD"
  | (T 21) => "IF"
  | (T 22) => "THEN"
  | (T 23) => "ELSE"
  | (T 24) => "ENDIF"
  | (T 25) => "WHILE"
  | (T 26) => "DO"
  | (T 27) => "ENDWH"
  | (T 28) => "EOF"
  | (T 29) => "AND"
  | (T 30) => "OR"
  | (T 31) => "TT"
  | (T 32) => "FF"
  | (T 33) => "NEG"
  | (T 34) => "NOT"
  | (T 35) => "MINUS"
  | (T 36) => "DIV"
  | (T 37) => "INT"
  | (T 38) => "BOOL"
  | (T 39) => "READ"
  | (T 40) => "WRITE"
  | (T 41) => "COM"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35)
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.E E1, E1left, E1right)) :: rest671)) => let
 val  result = MlyValue.START (fn _ => let val  (E as E1) = E1 ()
 in (SOME E)
end)
 in ( LrTable.NT 1, ( result, E1left, E1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.START (fn _ => (NONE
))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.CS CS1, _, CS1right)) :: ( _, ( MlyValue.DS 
DS1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
PROG1left, _)) :: rest671)) => let val  result = MlyValue.E (fn _ =>
 let val  ID1 = ID1 ()
 val  (DS as DS1) = DS1 ()
 val  (CS as CS1) = CS1 ()
 in (NODE("PROG" , DS, CS))
end)
 in ( LrTable.NT 0, ( result, PROG1left, CS1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.DS DS1, _, DS1right)) :: ( _, ( MlyValue.D 
D1, D1left, _)) :: rest671)) => let val  result = MlyValue.DS (fn _ =>
 let val  (D as D1) = D1 ()
 val  (DS as DS1) = DS1 ()
 in (NODE("DS",D,DS))
end)
 in ( LrTable.NT 2, ( result, D1left, DS1right), rest671)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.DS (fn _ => (NULL))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( _, _, EOC1right)) :: _ :: _ :: ( _, ( MlyValue.VL VL1
, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.D (fn _ => let val  (VL as VL1) = VL1 ()
 in (NODE("VAR",LEAF("INT"),VL))
end)
 in ( LrTable.NT 4, ( result, VAR1left, EOC1right), rest671)
end
|  ( 6, ( ( _, ( _, _, EOC1right)) :: _ :: _ :: ( _, ( MlyValue.VL VL1
, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.D (fn _ => let val  (VL as VL1) = VL1 ()
 in (NODE("VAR",LEAF("BOOL"),VL))
end)
 in ( LrTable.NT 4, ( result, VAR1left, EOC1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.VLE VLE1, _, VLE1right)) :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.VL (fn _ => let val  (ID as ID1) = ID1 ()
 val  (VLE as VLE1) = VLE1 ()
 in (NODE(lookup ID,NULL,VLE))
end)
 in ( LrTable.NT 5, ( result, ID1left, VLE1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.VLE VLE1, _, VLE1right)) :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, COM1left, _)) :: rest671)) => let
 val  result = MlyValue.VLE (fn _ => let val  (ID as ID1) = ID1 ()
 val  (VLE as VLE1) = VLE1 ()
 in (NODE(lookup ID,NULL,VLE))
end)
 in ( LrTable.NT 6, ( result, COM1left, VLE1right), rest671)
end
|  ( 9, ( rest671)) => let val  result = MlyValue.VLE (fn _ => (NULL))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 10, ( ( _, ( _, _, RCPAREN1right)) :: ( _, ( _, LCPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.CS (fn _ => (NULL))
 in ( LrTable.NT 3, ( result, LCPAREN1left, RCPAREN1right), rest671)

end
|  ( 11, ( ( _, ( _, _, RCPAREN1right)) :: ( _, ( MlyValue.CO CO1, _,
 _)) :: _ :: ( _, ( MlyValue.C C1, _, _)) :: ( _, ( _, LCPAREN1left, _
)) :: rest671)) => let val  result = MlyValue.CS (fn _ => let val  (C
 as C1) = C1 ()
 val  (CO as CO1) = CO1 ()
 in (NODE("SEQ",C,CO))
end)
 in ( LrTable.NT 3, ( result, LCPAREN1left, RCPAREN1right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.CO CO1, _, CO1right)) :: _ :: ( _, ( 
MlyValue.C C1, C1left, _)) :: rest671)) => let val  result = 
MlyValue.CO (fn _ => let val  (C as C1) = C1 ()
 val  (CO as CO1) = CO1 ()
 in (NODE("SEQ",C,CO))
end)
 in ( LrTable.NT 7, ( result, C1left, CO1right), rest671)
end
|  ( 13, ( rest671)) => let val  result = MlyValue.CO (fn _ => (NULL))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.C (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (NODE("SET",LEAF(lookup ID),EXP))
end)
 in ( LrTable.NT 8, ( result, ID1left, EXP1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( _, 
READ1left, _)) :: rest671)) => let val  result = MlyValue.C (fn _ =>
 let val  (ID as ID1) = ID1 ()
 in (NODE("READ",LEAF(lookup ID) , NULL))
end)
 in ( LrTable.NT 8, ( result, READ1left, ID1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.IEXP IEXP1, _, IEXP1right)) :: ( _, ( _, 
WRITE1left, _)) :: rest671)) => let val  result = MlyValue.C (fn _ =>
 let val  (IEXP as IEXP1) = IEXP1 ()
 in (NODE("WRITE",IEXP,NULL))
end)
 in ( LrTable.NT 8, ( result, WRITE1left, IEXP1right), rest671)
end
|  ( 17, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.CS CS2, _, _)
) :: _ :: ( _, ( MlyValue.CS CS1, _, _)) :: _ :: ( _, ( MlyValue.BEXP 
BEXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  
result = MlyValue.C (fn _ => let val  (BEXP as BEXP1) = BEXP1 ()
 val  (CS as CS1) = CS1 ()
 val  CS2 = CS2 ()
 in (NODE("ITE",BEXP,NODE("IF CS",CS1,CS2)))
end)
 in ( LrTable.NT 8, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 18, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.CS CS1, _, _)
) :: _ :: ( _, ( MlyValue.BEXP BEXP1, _, _)) :: ( _, ( _, WHILE1left,
 _)) :: rest671)) => let val  result = MlyValue.C (fn _ => let val  (
BEXP as BEXP1) = BEXP1 ()
 val  (CS as CS1) = CS1 ()
 in (NODE("WH",BEXP,NODE("DO",CS,NULL)))
end)
 in ( LrTable.NT 8, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.IEXP IEXP1, IEXP1left, IEXP1right)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (IEXP
 as IEXP1) = IEXP1 ()
 in (IEXP)
end)
 in ( LrTable.NT 9, ( result, IEXP1left, IEXP1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.BEXP BEXP1, BEXP1left, BEXP1right)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (BEXP
 as BEXP1) = BEXP1 ()
 in (BEXP)
end)
 in ( LrTable.NT 9, ( result, BEXP1left, BEXP1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.IT IT1, _, IT1right)) :: _ :: ( _, ( 
MlyValue.IEXP IEXP1, IEXP1left, _)) :: rest671)) => let val  result = 
MlyValue.IEXP (fn _ => let val  (IEXP as IEXP1) = IEXP1 ()
 val  (IT as IT1) = IT1 ()
 in (NODE("PLUS",IEXP,IT))
end)
 in ( LrTable.NT 10, ( result, IEXP1left, IT1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.IT IT1, _, IT1right)) :: _ :: ( _, ( 
MlyValue.IEXP IEXP1, IEXP1left, _)) :: rest671)) => let val  result = 
MlyValue.IEXP (fn _ => let val  (IEXP as IEXP1) = IEXP1 ()
 val  (IT as IT1) = IT1 ()
 in (NODE("MINUS",IEXP,IT))
end)
 in ( LrTable.NT 10, ( result, IEXP1left, IT1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.IT IT1, IT1left, IT1right)) :: rest671)) =>
 let val  result = MlyValue.IEXP (fn _ => let val  (IT as IT1) = IT1
 ()
 in (IT)
end)
 in ( LrTable.NT 10, ( result, IT1left, IT1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.IFAC IFAC1, _, IFAC1right)) :: _ :: ( _, ( 
MlyValue.IT IT1, IT1left, _)) :: rest671)) => let val  result = 
MlyValue.IT (fn _ => let val  (IT as IT1) = IT1 ()
 val  (IFAC as IFAC1) = IFAC1 ()
 in (NODE("TIMES",IT,IFAC))
end)
 in ( LrTable.NT 12, ( result, IT1left, IFAC1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.IFAC IFAC1, _, IFAC1right)) :: _ :: ( _, ( 
MlyValue.IT IT1, IT1left, _)) :: rest671)) => let val  result = 
MlyValue.IT (fn _ => let val  (IT as IT1) = IT1 ()
 val  (IFAC as IFAC1) = IFAC1 ()
 in (NODE("DIV",IT,IFAC))
end)
 in ( LrTable.NT 12, ( result, IT1left, IFAC1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.IFAC IFAC1, _, IFAC1right)) :: _ :: ( _, ( 
MlyValue.IT IT1, IT1left, _)) :: rest671)) => let val  result = 
MlyValue.IT (fn _ => let val  (IT as IT1) = IT1 ()
 val  (IFAC as IFAC1) = IFAC1 ()
 in (NODE("MOD",IT,IFAC))
end)
 in ( LrTable.NT 12, ( result, IT1left, IFAC1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.IFAC IFAC1, IFAC1left, IFAC1right)) :: 
rest671)) => let val  result = MlyValue.IT (fn _ => let val  (IFAC as 
IFAC1) = IFAC1 ()
 in (IFAC)
end)
 in ( LrTable.NT 12, ( result, IFAC1left, IFAC1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.NUME NUME1, NUME1left, NUME1right)) :: 
rest671)) => let val  result = MlyValue.IFAC (fn _ => let val  (NUME
 as NUME1) = NUME1 ()
 in (NUME)
end)
 in ( LrTable.NT 13, ( result, NUME1left, NUME1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.IFAC (fn _ => let val  (ID as ID1) = ID1
 ()
 in (LEAF(lookup ID))
end)
 in ( LrTable.NT 13, ( result, ID1left, ID1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.IEXP IEXP1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.IFAC (fn _ => let val  (IEXP as IEXP1) = IEXP1 ()
 in (IEXP)
end)
 in ( LrTable.NT 13, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 31, ( ( _, ( MlyValue.IFAC IFAC1, _, IFAC1right)) :: ( _, ( _, 
NEG1left, _)) :: rest671)) => let val  result = MlyValue.IFAC (fn _ =>
 let val  (IFAC as IFAC1) = IFAC1 ()
 in (NODE("~",IFAC,NULL))
end)
 in ( LrTable.NT 13, ( result, NEG1left, IFAC1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.BT BT1, _, BT1right)) :: _ :: ( _, ( 
MlyValue.BEXP BEXP1, BEXP1left, _)) :: rest671)) => let val  result = 
MlyValue.BEXP (fn _ => let val  (BEXP as BEXP1) = BEXP1 ()
 val  (BT as BT1) = BT1 ()
 in (NODE("OR",BEXP,BT))
end)
 in ( LrTable.NT 11, ( result, BEXP1left, BT1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.BT BT1, BT1left, BT1right)) :: rest671)) =>
 let val  result = MlyValue.BEXP (fn _ => let val  (BT as BT1) = BT1
 ()
 in (BT)
end)
 in ( LrTable.NT 11, ( result, BT1left, BT1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.BF BF1, _, BF1right)) :: _ :: ( _, ( 
MlyValue.BT BT1, BT1left, _)) :: rest671)) => let val  result = 
MlyValue.BT (fn _ => let val  (BT as BT1) = BT1 ()
 val  (BF as BF1) = BF1 ()
 in (NODE("AND",BT,BF))
end)
 in ( LrTable.NT 15, ( result, BT1left, BF1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.BF BF1, BF1left, BF1right)) :: rest671)) =>
 let val  result = MlyValue.BT (fn _ => let val  (BF as BF1) = BF1 ()
 in (BF)
end)
 in ( LrTable.NT 15, ( result, BF1left, BF1right), rest671)
end
|  ( 36, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.BF (fn _ => (LEAF("TT")))
 in ( LrTable.NT 16, ( result, TT1left, TT1right), rest671)
end
|  ( 37, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.BF (fn _ => (LEAF("FF")))
 in ( LrTable.NT 16, ( result, FF1left, FF1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.BF (fn _ => let val  (ID as ID1) = ID1 ()
 in (LEAF(lookup ID))
end)
 in ( LrTable.NT 16, ( result, ID1left, ID1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.COMP COMP1, COMP1left, COMP1right)) :: 
rest671)) => let val  result = MlyValue.BF (fn _ => let val  (COMP as 
COMP1) = COMP1 ()
 in (COMP)
end)
 in ( LrTable.NT 16, ( result, COMP1left, COMP1right), rest671)
end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.BEXP BEXP1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.BF (fn _ => let val  (BEXP as BEXP1) = BEXP1 ()
 in (BEXP)
end)
 in ( LrTable.NT 16, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 41, ( ( _, ( MlyValue.BF BF1, _, BF1right)) :: ( _, ( _, NOT1left
, _)) :: rest671)) => let val  result = MlyValue.BF (fn _ => let val 
 (BF as BF1) = BF1 ()
 in (NODE("NOT",BF,NULL))
end)
 in ( LrTable.NT 16, ( result, NOT1left, BF1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.IEXP IEXP2, _, IEXP2right)) :: _ :: ( _, ( 
MlyValue.IEXP IEXP1, IEXP1left, _)) :: rest671)) => let val  result = 
MlyValue.COMP (fn _ => let val  IEXP1 = IEXP1 ()
 val  IEXP2 = IEXP2 ()
 in (NODE("LT",IEXP1,IEXP2))
end)
 in ( LrTable.NT 17, ( result, IEXP1left, IEXP2right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.IEXP IEXP2, _, IEXP2right)) :: _ :: ( _, ( 
MlyValue.IEXP IEXP1, IEXP1left, _)) :: rest671)) => let val  result = 
MlyValue.COMP (fn _ => let val  IEXP1 = IEXP1 ()
 val  IEXP2 = IEXP2 ()
 in (NODE("LEQ",IEXP1,IEXP2))
end)
 in ( LrTable.NT 17, ( result, IEXP1left, IEXP2right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.IEXP IEXP2, _, IEXP2right)) :: _ :: ( _, ( 
MlyValue.IEXP IEXP1, IEXP1left, _)) :: rest671)) => let val  result = 
MlyValue.COMP (fn _ => let val  IEXP1 = IEXP1 ()
 val  IEXP2 = IEXP2 ()
 in (NODE("GT",IEXP1,IEXP2))
end)
 in ( LrTable.NT 17, ( result, IEXP1left, IEXP2right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.IEXP IEXP2, _, IEXP2right)) :: _ :: ( _, ( 
MlyValue.IEXP IEXP1, IEXP1left, _)) :: rest671)) => let val  result = 
MlyValue.COMP (fn _ => let val  IEXP1 = IEXP1 ()
 val  IEXP2 = IEXP2 ()
 in (NODE("GEQ",IEXP1,IEXP2))
end)
 in ( LrTable.NT 17, ( result, IEXP1left, IEXP2right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.IEXP IEXP2, _, IEXP2right)) :: _ :: ( _, ( 
MlyValue.IEXP IEXP1, IEXP1left, _)) :: rest671)) => let val  result = 
MlyValue.COMP (fn _ => let val  IEXP1 = IEXP1 ()
 val  IEXP2 = IEXP2 ()
 in (NODE("NEQ",IEXP1,IEXP2))
end)
 in ( LrTable.NT 17, ( result, IEXP1left, IEXP2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.IEXP IEXP2, _, IEXP2right)) :: _ :: ( _, ( 
MlyValue.IEXP IEXP1, IEXP1left, _)) :: rest671)) => let val  result = 
MlyValue.COMP (fn _ => let val  IEXP1 = IEXP1 ()
 val  IEXP2 = IEXP2 ()
 in (NODE("EQ",IEXP1,IEXP2))
end)
 in ( LrTable.NT 17, ( result, IEXP1left, IEXP2right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.NUM NUM1, _, NUM1right)) :: ( _, ( _, 
PLUS1left, _)) :: rest671)) => let val  result = MlyValue.NUME (fn _
 => let val  (NUM as NUM1) = NUM1 ()
 in (LEAF(Int.toString(NUM)))
end)
 in ( LrTable.NT 14, ( result, PLUS1left, NUM1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.NUME (fn _ => let val  (NUM as NUM1) =
 NUM1 ()
 in (LEAF(Int.toString(NUM)))
end)
 in ( LrTable.NT 14, ( result, NUM1left, NUM1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.NUM NUM1, _, NUM1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.NUME (fn _
 => let val  (NUM as NUM1) = NUM1 ()
 in (LEAF("~"^Int.toString(NUM)))
end)
 in ( LrTable.NT 14, ( result, MINUS1left, NUM1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun LCPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun RCPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun PROG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun BLK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun EOC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun ISEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun COM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
