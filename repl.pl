start:- 
 A=[a1,b1,c1,d1,e1,f1,g1,h1,a2,b2,c2,d2,e2,f2,g2,h2],
 replace2(A,5,'XX',List),
 writeln(List).
 
tr(Method, K) :-
    length(L, K),
    K1 is K - 1,
    time(call(Method, L, K1, test, R)),
    assertion(nth1(K, R, test)).


replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

replace1(L, I, X, R) :-
    Dummy =.. [dummy|L],
    J is I + 1,
    nb_setarg(J, Dummy, X),
    Dummy =.. [dummy|R].

replace2(L, I, X, R) :-
    Dummy =.. [dummy|L],
	write('L is :'),writeln(L),
	write('Dummy is :'),writeln(Dummy),
	J is I + 1,
    setarg(J, Dummy, X),
	write('Dummy is :'),writeln(Dummy),	
    Dummy =.. [dummy|R],
	write('Dummy is :'),writeln(Dummy).