:-dynamic(object/3).

objects_ids(Classid, Objid) :-
	object(Objid, _, Classid).
objects_ids(Classid, Objid) :-
	generalization(_, Z, Classid),
	objects_ids(Z, Objid).
all_objects_ids(Classid, IDS) :-
	findall(Y, objects_ids(Classid, Y), IDS).
	
	
obj(A,B,C):-
	assert(object(A,B,C)),
	write("Hello").
	
class_(A):-
	assert(class(A,_,_)).
	

parents(Superid, Subid) :-
	generalization(_, Subid, Superid).
parents(Superid, Subid) :-
	generalization(_, Subid, X),
	parents(Superid, X).
all_parents(Subid, IDS) :-
	findall(Y, parents(Y, Subid), IDS).
	
generalize(X,Y):-
	assert(generalization(_,X,Y)).
	
list_reps([],[]).
list_reps([X|Xs],Ds1) :-
	x_reps_others_fromlist(X,Ds,Os,Xs),
	list_reps(Os,Ds0),
	append(Ds,Ds0,Ds1).
	
	
x_reps_others_fromlist(_X,[],[],[]).
x_reps_others_fromlist(X,[X|Ds],Os,[X|Ys]) :-
	x_reps_others_fromlist(X,Ds,Os,Ys).
x_reps_others_fromlist(X,Ds,[Y|Os],[Y|Ys]) :-
	dif(Y,X),
	x_reps_others_fromlist(X,Ds,Os,Ys).

canGoTo(X, N, Nodes) :-
	member(X2, [X|Nodes]),
	generalization(_, X2,X1),
	\+ member(X1, Nodes),
	canGoTo(X, N, [X1|Nodes]).

canGoTo(_, N, N).

canGoTo(X, Nodes) :-
	canGoTo(X, Nodes, []).
inheritSelf(X) :-
	canGoTo(X, Nodes), member(X, Nodes), !.
	
member(X, [X|_]).
member(X, [_|Tail]) :-
	member(X, Tail).
	
	
object_rule(Objid, Classid) :-
	object(Objid, _, Classid),
	class(Classid, _, _),
	write("Object: "), write(Objid), nl,
	write("Class: "), write(Classid).

assoc_exist(Msgid, Sndobjid, Recobjid,ClassA, ClassB):-
	link(Msgid, Sndobjid, Recobjid),
	object(Sndobjid, _, ClassA),
	object(Recobjid, _, ClassB),
	association(_, ClassA, ClassB),
	association(_, ClassB, ClassA).
	
link(_,A,B):-
	object(A,_,C1),
	object(B,_,C2),
	class(C1,_,_),
	class(C2,_,_).
	
	
assoc(A,B):-
	class(A,_,_),
	class(B,_,_),
	write("Association added"),
	assert(association(_,A,B)).

gen_rule(Superid, Subid) :-
	generalization(_, Superid, Subid),
	all_objects_ids(Subid, IDS), write(IDS), nl,
	all_objects_ids(Superid, IDS2), write(IDS2), nl,
	subset(IDS, IDS2).
