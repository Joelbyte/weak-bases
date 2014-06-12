:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2014/06/12,
		comment is 'Unit tests for the "weak-bases" appliation.'
	]).

	cover(relations).
	cover(graphic).
	cover(operators).

	test(weak_bases_1) :-
		relations::weak_base(br/_, R),
		R == [[0,0,0,0,1,1,1,1],[0,0,1,1,0,0,1,1],[0,1,0,1,0,1,0,1]].

	test(weak_bases_2) :-
		R1 = [[0,0,1],[0,1,0],[1,0,0],[1,1,0],[1,0,1],[0,1,1]],
		relations::smallest_co_clone(R1, R2),
		R2 == in2/3.

:- end_object.
