:- object(graphic).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2014-03-03,
		comment is 'Predicates for displaying graphics.'
	]).

   :- public(draw/1).

path_window(Ps) :-
	new(F, frame('Path Window!')),
	send(F, append(new(D, dialog))),
	new(P, path(smooth, 5)),
	send(P, points, Ps),
	send(D, append(P)),
	send(D, append(button(quit,
			      message(F, destroy)))),
	send(F, open).

    row_to_points([], _, []).
    row_to_points([0|Xs], X-Y, Ps) :-
        X1 is X + 1,
		row_to_points(Xs, X1-Y, Ps).
    row_to_points([0|Xs], X-Y, [P|Ps]) :-
        X1 is X + 1,
		new(P, point(X, Y)),
		row_to_points(Xs, X1-Y, Ps).


matrix_to_points([], _, []).
matrix_to_points([R|Rs], Y, Ps) :-
    row_to_points(R, 0-Y, Ps1),
	Y1 is Y+1,
	matrix_to_points(Rs, Y1, Ps2),
	list::append(Ps1, Ps2, Ps).


:- end_object.
