:- object(operators).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2014-03-03,
		comment is 'Predicates working on operations.'
	]).

   :- public(operator/3).
   :- public(close_relation/3).
   :- public(closed_relation/2).

   operator(c0/1, [_], 0).
   operator(c1/1, [_], 1).

   operator(id/1, [X], X).

   operator(not/1, [X], Y) :-
       Y is X xor 1.

   operator(imp/2, [X,Y], R) :-
       R is (X xor 1) \/ Y.
   operator(iff/2, [X,Y], R) :-
       R is ((X xor Y) xor 1).
   operator(or/2, [X, Y], Z) :-
       Z is X \/ Y.

   operator(eq/2, [X, Y], Z) :-
       (X = Y -> Z = 1; Z = 0).
   
   operator(xor/2, [X, Y], Z) :-
       Z is X xor Y.
   operator(xor/3, [X, Y, Z], W) :-
       W is (X xor Y) xor Z.

   operator(il3/3, [X, Y, Z], W) :- 
       W is ((X xor Y) xor Z) xor 1.

   operator(id/3, [X,Y,Z], R) :-
       R is (X /\ (Y xor 1)) \/ (X /\ (Z xor 1)) \/ ((Y xor 1) /\ (Z xor 1)).
   operator(id1/3, [X,Y,Z], R) :-
       operator(not/1, [Z], Zn),
       R is (X /\ Y) \/ (X /\ Zn) \/ (Y /\ Zn).
   operator(id2/3, [X,Y,Z], R) :-
       R is (X /\ Y) \/ (Y /\ Z) \/ (X /\ Z).

   operator(s10/3, [X,Y,Z], R) :-
       R is X /\ (Y \/ Z).
   operator(s12/3, [X,Y,Z], R) :-
       operator(not/1, [Z], Zn),
       R is X /\ (Y \/ Zn).
   operator(s00/3, [X,Y,Z], R) :-
       R is X \/ (Y /\ Z).
   operator(s02/3, [X,Y,Z], R) :-
       operator(not/1, [Z], Zn),
       R is X \/ (Y /\ Zn).
   operator(s1/2, [X,Y], R) :-
       R is X /\ (Y xor 1).
   operator(s0/2, [X,Y], R) :-
       R is (X xor 1) \/ Y.
   operator(r1/2, [X,Y], R) :-
       R is (X xor Y) xor 1.
   operator(r2/3, [X,Y,Z], R) :-
       R is X /\ (Y xor Z xor 1).

   operator(hn/3, [X,Y,Z], R) :-
       R is (Y /\ Z) \/ (X /\ Z) \/ (X /\ Y).
   operator(hn/4, [X,Y,Z,W], R) :-
       R is (Y /\ Z /\ W) \/ (X /\ Z /\ W) \/ (X /\ Y /\ W) \/ (X /\ Y /\ Z).
   operator(hn/5, [X,Y,Z,W,V], R) :-
       R is (Y /\ Z /\ W /\ V) \/ (X /\ Z /\ W /\ V)
	     \/ (X /\ Y /\ W /\ V) \/ (X /\ Y /\ Z /\V) \/ (X /\ Y /\ Z /\ W).

   operator(dhn/3, [X,Y,Z], R) :-
       R is (((Y xor 1) /\ (Z xor 1)) \/ ((X xor 1) /\ (Z xor 1)) \/ ((X xor 1) /\ (Y xor 1))) xor 1.
   operator(dhn/4, [X,Y,Z,W], R) :-
       R is ((((Y xor 1) /\ (Z xor 1) /\ (W xor 1)) \/
	          ((X xor 1) /\ (Z xor 1) /\ (W xor 1)) \/
			  ((X xor 1) /\ (Y xor 1) /\ (W xor 1)) \/
			  ((X xor 1) /\ (Y xor 1) /\ (Z xor 1)))
			  xor 1).
   operator(dhn/5, [X,Y,Z,W,V], R) :-
       R is ((((Y xor 1) /\ (Z xor 1) /\ (W xor 1) /\ (V xor 1))
	   \/ ((X xor 1) /\ (Z xor 1) /\ (W xor 1) /\ (V xor 1))
	     \/ ((X xor 1) /\ (Y xor 1) /\ (W xor 1) /\ (V xor 1)) \/
		 ((X xor 1) /\ (Y xor 1) /\ (Z xor 1) /\ (V xor 1))
		 \/ ((X xor 1) /\ (Y xor 1) /\ (Z xor 1) /\ (W xor 1))) xor 1).

   operator(and/_, Xs, R) :-
       and(Xs, R).

   duals([], []).
   duals([X|Xs], [Y|Ys]) :-
      Y is X xor 1,
   	  duals(Xs, Ys).

   and([], 1).
   and([X|Xs], R) :-
       and(Xs, R0),
	   R is X /\ R0.

   hn(Ls, _, [], R) :-
       and(Ls, R).
   hn(Ls, Xi, [Xj|Rs], R) :-
	   hn([Xi|Ls], Xj, Rs, R0),
	   and(Ls, R1),
	   and([Xj|Rs], R2),
	   R2 is R1 /\ R2,
	   R is R0 \/ R2.

   symmetric(hn/_).
   symmetric(dhn/_). 

   arity(_/N, N).
   
   members([], _).
   members([X|Xs], Ys) :-
       list::member(X, Ys),
	   members(Xs, Ys).

   members_sym([], _).
   members_sym([X|Xs], Zs) :-
       list::append(_, [X|Rs], Zs),
	   members_sym(Xs, [X|Rs]).

   members_sym([], [], []).
   members_sym(Xs, [D|Ds], Zs) :-
       (  Xs = [D|Xs1],
	      members_sym(Xs1, Zs)
        ; members_sym(Xs, Ds, Zs)).

   apply_op1([], _, []).
   apply_op1([C|Cs], Op, [C1|C1s]) :-
       operator(Op, C, C1),
   	   apply_op1(Cs, Op, C1s).

   apply_op(Xs, Op, Ys) :-
       matrix::transpose(Xs, Cs),
   	   apply_op1(Cs, Op, Ys),
       !.

   apply_hn([[],[],[],[],[]], []).
   apply_hn([[X|X1s], [Y|X2s], [Z|X3s], [W|X4s], [V|X5s]], [R|Rs]) :-
        R is ((((Y xor 1) /\ (Z xor 1) /\ (W xor 1) /\ (V xor 1))
	   \/ ((X xor 1) /\ (Z xor 1) /\ (W xor 1) /\ (V xor 1))
	     \/ ((X xor 1) /\ (Y xor 1) /\ (W xor 1) /\ (V xor 1)) \/
		 ((X xor 1) /\ (Y xor 1) /\ (Z xor 1) /\ (V xor 1))
		 \/ ((X xor 1) /\ (Y xor 1) /\ (Z xor 1) /\ (W xor 1))) xor 1), !,
		 apply_hn([X1s, X2s, X3s, X4s, X5s], Rs).

   next([], _, _,Rs1, Rs1).
   next([Op|Ops], Rs, D, Rs1, Rs2) :-
       arity(Op, N),
	   write('Op is: '), write(Op),nl,
	   (symmetric(Op) ->
       findall(Ys, (length(Xs, N),
   	                members_sym(Xs, D, Rs),
					%write('Testing the tuple: '), write(Xs),nl,
                    apply_op(Xs, Op, Ys)),
   				    %apply_hn(Xs, Ys)),
					%write('Ys is: '), write(Ys),nl),
   					Rs1,
   					Rs3)
       ; 
       findall(Ys, (length(Xs, N),
   	                members(Xs, Rs),
					%write('Testing the tuple: '), write(Xs),nl,
   				    apply_op(Xs, Op, Ys)),
   					Rs1,
   					Rs3)),
	   !,
   	   next(Ops, Rs, D, Rs3, Rs2).

   next(Rs, D, Ops, Rs1) :-
       next(Ops, Rs, D, Rs0, []),
	   sort(Rs0, Rs1).

   find_fixpoint(Rs, D0, Ops, Fix) :-
       next(Rs, D0, Ops, Rs1),
       set::union(Rs, Rs1, Rs2, D),
	   list::length(Rs2, L1),
	   list::length(D, L2),
	   write('Total amount of elements is now '), write(L1),nl,
	   write('New elements are '), write(L2),nl,
	   (  set::empty(D) ->
	      Fix = Rs2
       ;  find_fixpoint(Rs2, D, Ops, Fix)
       ).

   find_fixpoint(Rs, Ops, Fix) :-
       find_fixpoint(Rs, Rs, Ops, Fix).

   close_relation(R, Ops, R1) :-
       sort(R, R0),
       find_fixpoint(R0, Ops, R1).
 
   %True if R is closed under Ops.
   closed_relation(R, Ops) :-
       sort(R, R0),
	   \+((list::member(Op, Ops),
	   	  arity(Op, N),
	      length(Xs, N),
   	      members(Xs, R0),
	      apply_op(Xs, Op, Ys),
		  \+ list::member(Ys, R0))).

:- end_object.
