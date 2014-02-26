:- object(matrix).

   :- public(add_as_columns/3).
   :- public(remove_duplicate_columns/2).
   :- public(remove_duplicate_rows/2).
   :- public(collapse_rows/4).
   :- public(add_as_row/3).
   :- public(tuple/1).
   :- public(write_matrix/1).
   :- public(write_png/2).
   :- public(first/3).
   :- public(transpose/2).
   :- public(difference/3).

   add_as_columns(Rs, [], Rs).
   add_as_columns(Rs, [C|Cs], Rs1) :-
       add_as_column(Rs, C, Rs0),
	   add_as_columns(Rs0, Cs, Rs1).

   add_as_column([], [], []).
   add_as_column([R|Rs], [X|Xs], [R1|R1s]) :-
       list::append(R, [X], R1),
	   add_as_column(Rs, Xs, R1s).

   add_as_row(Rs, Xs, Rs1) :-
       list::append(Rs, [Xs], Rs1).

   first([[F|Xs]], [F], [Xs]).
   first([[F|Xs]|Ys], [F|Fs], [Xs|Zs]) :-
       Ys \== [],
       first(Ys, Fs, Zs). 

   transpose([], []).
   transpose([[]|_], []).
   transpose(Rs, [C|Cs]) :-
       Rs \= [],
       first(Rs, C, Rest),
	   transpose(Rest, Cs).

   tuple([]).
   tuple([X|Xs]) :-
	   tuple(Xs),
       between(0,1,X).

   remove_duplicate_rows(Rs, Rs1) :-
       sort(Rs, Rs1).

   remove_duplicate_columns(Rs, Rs2) :-
       transpose(Rs, Rs0),
	   sort(Rs0, Rs1),
	   transpose(Rs1, Rs2).

   write_row([]).
   write_row([0|Xs]) :-
       write('0 '),
	   write_row(Xs).
   write_row([1|Xs]) :-
       write('1 '),
	   write_row(Xs).

   write_matrix([]).
   write_matrix([R|Rs]) :-
       write_row(R), nl,
	   write_matrix(Rs).

   write_row([], _).
   write_row([X|Xs], S) :-
       write(S, X),
	   write(S, '	'),
	   write_row(Xs, S).

   write_png1([], _).
   write_png1([R|Rs], S) :-
       write_row(R, S), nl(S),
	   write_png1(Rs, S).

   write_png(Rs, F) :-
       Rs \= [],
       open(F, write, S),
	   write_png1(Rs, S),
	   close(S),
	   atom_concat('matrix2png -data ', F, C0),
	   atom_concat(C0, ' -bkgcolor white -mincolor green -maxcolor red -missingcolor grey -size 4:4 > ', C1),
	   atom_concat(C1, F, C2),
	   atom_concat(C2, '.png', C),
	   shell(C).

   collapse_rows([], [], [], []).
   collapse_rows([R|Rs], [X|Xs], [Y|Ys], Rs1) :-
       (   X = Y ->
           Rs1 = [R|Rs2]
        ;  Rs1 = Rs2),
	   collapse_rows(Rs, Xs, Ys, Rs2).

   identify(Rs, C1, C2, Rs1) :-
       collapse_rows(Rs, C1, C2, Rs1).
	   

   difference([], [], 0).
   difference([X|Xs], [Y|Ys], D) :-
       difference(Xs, Ys, D0), 
       (   X = Y ->
	       D is D0
       ;   D is D0 + 1).

:- end_object.
