% integer binart tree in prolog

ibt(empty).
ibt(node(N,L,R)) :- integer(N),ibt(L),ibt(R).

% predicates to size of the binary tree.
% recursion on left and right parts
size(empty,0).
size(node(A,L,R),N) :- ibt(node(A,L,R)),size(L,X),size(R,Y),N is 1+X+Y.

% predicates to height of binary tree
% recursion ono left and right branches of node
height(empty,0) :-!.
height(node(A,L,R),H) :- ibt(node(A,L,R)),height(L,H1),height(R,H2),H1>=H2,H is 1+H1 ,!.
height(node(A,L,R),H) :- ibt(node(A,L,R)),height(L,H1),height(R,H2),H2>H1,H is 1+H2 ,!.

% list operations :
% length of list calculates length of the list from simple recursive function , time complexity (O(len(list))).
% concat_lists concates two list one after other , time complexity ( O(len(list1))).
% split_list	splits a list into two lists of equal halfs, if length is odd len(1 st halsf)+1=len(2 nd half).
len_list([],0).
len_list([_|T],L):-len_list(T,L1),L is 1+L1.

concat_lists([],L,L).
concat_lists([E|L1],L2,[E|L3]) :- concat_lists(L1,L2,L3).

split_list(L,L1,L2):- len_list(L1,X1),len_list(L,X2),X1 is X2//2,concat_lists(L1,L2,L). 

% insertion  sorting algorithm  to sort lists. time complexity O(n*n).
list_insert(X, [], [X]) :- !.
list_insert(X, [X1|L1], [X, X1|L1]):- X=<X1 ,!.
list_insert(X, [X1|L1], [X1|L]):- list_insert(X, L1, L).
sort_list([], []) :- !.
sort_list([X|L], S):- sort_list(L, S1), list_insert(X, S1, S).

% preorder of a BT recursive way of left and right subtrees.
preorder(empty,[]).
preorder(node(A,X,Y),L) :- ibt(node(A,X,Y)),preorder(X,L1),preorder(Y,L2),concat_lists([A|L1],L2,L).

% inorder of a BT recursive way of left and right subtrees.
inorder(empty,[]).
inorder(node(A,X,Y),L) :- ibt(node(A,X,Y)),inorder(X,L1),inorder(Y,L2),concat_lists(L1,[A|L2],L).

% postorder of a BT recursive way of left and right subtrees.
postorder(empty,[]).
postorder(node(A,X,Y),L) :- ibt(node(A,X,Y)),postorder(X,L1),postorder(Y,L2),concat_lists(L1,L2,L3),concat_lists(L3,[A],L).


% stack methods for node stacks (push,pop,top,is_empty are only useful in this assignment so they are implemented.
stack([]).
stack([H|T]) :- ibt(H),stack(T).
stack_isempty([]).
stack_push(X,empty,X).
stack_push([],X,[X]) :-ibt(X).
stack_push(L,X,Y):-concat_lists([X],L,Y).
stack_pop([X],[]):-ibt(X).
stack_pop([H|T],T):-ibt(H),stack(T).
stack_top([X],X):-ibt(X).
stack_top([H|T],H):-ibt(H),stack(T). 

% Helper Function gives the absolute value of a given number
abs(X, Y) :- X < 0, Y is -X. 
abs(X, X) :- X >= 0. 

% tail recursive preorder of BT algorithm is written by using stacks.
% pop the top of the stack and push its right and left concat the popped node to answer,until the stack is empty. 
f([],[]) :- !.
f(S,X) :- stack_top(S,node(A,L,R)),stack_pop(S,S2),stack_push(S2,R,S3),stack_push(S3,L,S1),f(S1,L1),concat_lists([A],L1,X).

trPreorder(empty,[]) :- !.
trPreorder(node(A,L,R),X) :- f([node(A,L,R)],X), !.

% tail recursive version of inorder of BT algorithm by using stacks.
% pop the top of stack and push its left and right concat the popped node to answer,until the stack is empty. 
g([],[]) :- !.
g(S,X) :- stack_top(S,node(A,L,R)),stack_pop(S,S2),stack_push(S2,L,S3),stack_push(S3,R,S1),g(S1,L1),concat_lists(L1,[A],X).

trPostorder([],[]) :- !.
trPostorder(node(A,L,R),X) :- g([node(A,L,R)],X) , !.

% tail recursive version of inorder of a BT using stacks.
% the second argument is the pointer to the node we are dealing with . first argument is the stack and the last argument is the list which is inorder of BT (initial pointer).
h([],empty,[]) :- !.
h(S,empty,X) :- stack_top(S,node(A,_,R)),stack_pop(S,S1),h(S1,R,L1),concat_lists([A],L1,X).
h(S,node(A,L,R),X):-stack_push(S,node(A,L,R),S1),h(S1,L,X).

trInorder([],[]) :- !.
trInorder(node(A,L,R),X) :- h([],node(A,L,R),X),!.

% Euler Tour algorithm based on the given example in assignment.
%start from root--go to left--perform Euler tour--come to root--go to right--perform Euler Tour--come to root.
eulerTour(empty,[]) :- !.
eulerTour(node(A,empty,empty),[A,A,A]) :- !.
eulerTour(node(A,L,R),X) :- eulerTour(L,X1),eulerTour(R,X2),concat_lists([A],X1,L1),concat_lists(L1,[A],L2),concat_lists(L2,X2,L3),concat_lists(L3,[A],X),!.

% heper functions to get inorder preorder postorder from Euler Tour.
clear(H,[H|T],T).
clear(H,[X|T],V):- H =\= X,clear(H,T,W),concat_lists([X],W,V).

erase(H,L,1,P):-clear(H,L,P).
erase(H,[H|T],N,P):-M is N-1,erase(H,T,M,Q),concat_lists([H],Q,P).
erase(X,[H|T],N,P):-H =\= X,erase(X,T,N,Q),concat_lists([H],Q,P).

edit([H],L2,N,L):-erase(H,L2,N,L).
edit([H|T],L2,N,L):-edit(T,L2,N,L1),erase(H,L1,N,L).

% delete the second and third occurances of every number in euler tour we will get preorder.
% delete the first and last occurances of every number in euler tour we will get inorder.
% delete the first and second occurances of every number in euler tour to get post order
preET(empty,[]) :- !.
preET(node(A,L,R),X):- preorder(node(A,L,R),P),eulerTour(node(A,L,R),E),edit(P,E,2,X1),edit(P,X1,2,X),!.

inET(empty,[]) :- !.
inET(node(A,L,R),X):-preorder(node(A,L,R),P),eulerTour(node(A,L,R),E),edit(P,E,1,X1),edit(P,X1,2,X),!.

postET(empty,[]) :- !.
postET(node(A,L,R),X):-preorder(node(A,L,R),P),eulerTour(node(A,L,R),E),edit(P,E,1,X1),edit(P,X1,1,X),!.


% Recursive function to convert BT to string.
toString(empty,'()') :- !.
toString(node(A,L,R),S) :-toString(L,S6),toString(R,S7), number_string(A,X),string_concat('(',X,S1),string_concat(S1,',',S2),string_concat(S2,S6,S3),string_concat(S3,',',S4),string_concat(S4,S7,S5),string_concat(S5,')',S).

% Recursion on left and right subtrees -- calculating heights and checking at every node.
isBalanced(empty).
isBalanced(node(A,L,R)) :- integer(A),height(L,H1),height(R,H2),abs(H1-H2,H), H =< 1 , isBalanced(L),isBalanced(R).

% Checking at every node wether it is in BST format node<right node , left node<node.
isBST(empty) :- !.
isBST(node(_,empty,empty)) :- !.
isBST(node(A,empty,node(A1,L1,R1))) :- A =< A1,isBST(node(A1,L1,R1)),!.
isBST(node(A,node(A1,L1,R1),empty)) :- A>=A1,isBST(node(A1,L1,R1)),!.
isBST(node(A,node(A1,L1,R1),node(A2,L2,R2))) :- A>=A1,A=<A2,isBST(node(A1,L1,R1)),isBST(node(A2,L2,R2)).

% recursive search on both left and right parts. also searches there are multiple elemets in tree.
lookup(N,node(A,L,R)) :-  N is A;N<A,lookup(N,L);N>A,lookup(N,R).

% Make a simple BST insertions and then balances by getting inorder and using makeBST.
insert(N,empty,node(N,empty,empty)) :- !.
insert(N,node(A,L,R),X) :- N=<A,insert(N,L,L1),inorder(node(A,L1,R),Y),makeBST(Y,X),!.
insert(N,node(A,L,R),X) :- N>A,insert(N,R,R1),inorder(node(A,L,R1),Y),makeBST(Y,X),!.

% Using KD trees algorithms , creates a BST from sorted list on simple recursion on left and right sides.
makeBST([],empty) :- !.
makeBST(L,node(A,X,Y)):-sort_list(L,L3),split_list(L3,L1,[H|T]),A is H, makeBST(L1,X),makeBST(T,Y),!.

% checks wether a node is leaf in a BST.
isLeaf(N,node(N,empty,empty)) :- !.
isLeaf(N,node(A,L,_)) :- N<A,isLeaf(N,L).
isLeaf(N,node(A,_,R)) :- N>A,isLeaf(N,R).

% helper function used to get inorder of and element and also used to replace with an element to other in a BST.
getnext(H,[H,H1|_],H1).
getnext(H,[H],H).
getnext(X,[H|T],L):-H =\= X,getnext(X,T,L).  
ios(N,node(A,L,R),N1) :-  inorder(node(A,L,R),L1),getnext(N,L1,N1).
replace(X,node(X,L,R),Y,node(Y,L,R)).
replace(X,node(A,L,R),Y,node(A,L1,R)) :- X<A,replace(X,L,Y,L1).
replace(X,node(A,L,R),Y,node(A,L,R1)) :- X>A,replace(X,R,Y,R1).


% delete a node if its a leaf and deletes a node by replacing it with inorder suffix and then from inorder it creates a balanced BST.

delete(N,node(N,empty,empty),empty) :- !.
delete(N,node(N,node(A,L,R),empty),node(A,L,R)) :- !.
delete(N,node(N,empty,node(A,L,R)),node(A,L,R)) :- !.
delete(N,node(A,L,R),X):-isLeaf(N,node(A,L,R)),N<A,delete(N,L,L1),inorder(node(A,L1,R),Y),makeBST(Y,X),!.
delete(N,node(A,L,R),X):-isLeaf(N,node(A,L,R)),N>A,delete(N,R,R1),inorder(node(A,L,R1),Y),makeBST(Y,X),!.
delete(X,node(N,L,R),B) :-ios(X,node(N,L,R),Y), delete(Y,node(N,L,R),M),replace(X,M,Y,A),inorder(A,C),makeBST(C,B),!.


% deletion and insertion makes a Balaced BST from BST1 to BST2.

