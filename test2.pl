length_(Length, List) :- length(List, Length).

start:-
	length(List, 9),
	write('List1 is :'),writeln(List),
	maplist(length_(9), List),
	initarr(99,List),
	write('List2 is :'),writeln(List).

initarr(Value,[]).
initarr(Value,[Row|Rest]):-
	init(Value,Row),
	initarr(Value,Rest).
	
init(Value,[]):-writeln('base case'),Head is Value.

init(Value,[Head|Tail]):-
Head is Value,
write('Value is :'),writeln(Value),
init(Value,Tail).
		
		
start3:-
 A=[a1,b1,c1,d1,e1,f1,g1,h1],
 writeln(A),
 B = A,
 writeln(B).