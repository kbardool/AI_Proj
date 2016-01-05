% show(+Board)
% Show the board to current output.
show:-

	Dim is 8,	
	length(List, Dim),
	maplist(length_(Dim), List),
	initarr('  ',List),
	showarr(List,Dim),
	% getrow(5,A,Out),write('Row 5 is :  '),writeln(Out),
	% getcell(5,6,A,Out2),write('Cell (5,6) is :  '),writeln(Out2),
	% getcol(4,A,Out3),write('Col 4 is :  '),writeln(Out3),
	% setrow(2,[a9,b9,c9,d9,e9,f9,g9,h9],A,NewA), show(NewA,8),
	writeln('Set cell values2'),	
	
	setcell(4,4,'OO',List,List2),
	setcell(4,5,'XX',List2,List3),
	setcell(5,4,'XX',List3,List4),
	setcell(5,5,'OO',List4,List5),
	% setcell(1,1,'ZZ',List5,List6),
	% setcell(6,Dim,'PP',List6, List7),

	showarr(List5,Dim),
	writef(" Array list is : %w \n",[List5]),
	writeln('..End of Story'),!.

showarr(List,Dim):-
 
	nl,nl,
	showheader(Dim,0),
	showtable(List,Dim,Dim).

showheader(Dim,Dim):-nl.
showheader(Dim,P):-
	P < Dim,	
    ( (P =:= 0, write('      |') ); (P =\= 0)),
	P2 is P + 1,
	writef("  %w  ",[P2]), 
	showheader(Dim,P2).
	
showtable(_,0,Col):-	
	writef('      -%r \n',['-----',Col]).
showtable([L|Rest],Row,Col):-
	% writef("Show table, Row/Col is: %w/%w \n",[Row,Col]),
	Row > 0 ,
	writef('      -%r \n',['-----',Col]),
	write('L:'),write(Row), write('   |'),
	showrow(L,Col),	
	Row2 is Row - 1,
	showtable(Rest,Row2,Col).
	
	
showrow(_,0):-	nl.
showrow([H|Rest],Col):-
	Col > 0,
	write(' '),write(H),write(' |'),
	% ((Pos > 1, write('|'));(Pos == 1)),
	Col2 is Col - 1,
	showrow(Rest, Col2).

  
getcell(_,_,[],'').
getcell(X,Y,Matrix,Out):-
	X > 0, Y > 0,
	getrow(X,Matrix,Row),
	getcellinrow(Y,Row,Out).

	
setcell(0,0,_,Matrix,Matrix).
setcell(R,C,Value,Matrix,NewMatrix):-
	% writef("Set Cell, X/Y is: %w/%w \n",[R,C]),
	R > 0 , C > 0,
	% NewMatrix = Matrix,
	getrow(R,Matrix,Row),
	setcellinrow2(C,Value,Row,NewRow),
	setrow(R,NewRow,Matrix,NewMatrix).
	% writeln("setcell/SetRow complte New Matrix is:"),writeln(NewMatrix).
  
getrow(1,[Line|_],Line).
	% writeln('Get Row base case').
getrow(X,[_|Rest],Out):-
	X > 1, 	X2 is X -1,
	% write("Get Row , Row is:"),writeln(X),
	getrow(X2,Rest,Out).

	
setrow(1,NewRow,[_|Rest],[NewRow|Rest]).
	% :-	writeln('Set Row base case').
setrow(X,NewRow,[Old|Rest],Out):-
	X > 1, X2 is X -1, 
	% write("Set Row , Row is:"),writeln(X),
	setrow(X2,NewRow,Rest,Out2),
	% write("Return from Set Row Recursive, Row is:"),writeln(X2),
	% write('Old is :'),writeln(Old),
	% write('Out2 is :'),writeln(Out2),write('Out is :'),writeln(Out),
	append([Old],Out2,Out).
	 % write('Out is :'),writeln(Out).
	
	
getcellinrow(1,[H|_],[H]):-!.
getcellinrow(Y,[_|Rest],Out):-
	Y2 is Y - 1,
	getcellinrow(Y2,Rest,Out).
	
setcellinrow(1,Elem,[_|Rest],[Elem|Rest]).	
setcellinrow(Y,Elem,[Old|Rest],Out):-
	 Y2 is Y - 1,
	 setcellinrow(Y2,Elem,Rest,Out2),
	 append([Old],Out2,Out).
	 
setcellinrow2(I, X, L, R) :-
    Dummy =.. [dummy|L],
	% write('L is :'),writeln(L),
	% write('Dummy is :'),writeln(Dummy),
	J is I ,
    setarg(J, Dummy, X),
	% write('Dummy is :'),writeln(Dummy),	
    Dummy =.. [dummy|R].
	% write('Dummy is :'),writeln(Dummy).	 
	
getcol(Col,Matrix,Out):-
   Row is 8,
   % write('Out is '),write(Out),
   getcolcell(Row,Col,Matrix,Out).
   % append(Cell,Out,Out).


getcolcell(0,_,_,[]).
getcolcell(Row,Col,Matrix,Out):-
	Col > 0, 	
	getrow(Row,Matrix,Line),
	getcellinrow(Col,Line,Cell),
	Row2 is Row - 1,
	getcolcell(Row2,Col,Matrix,Cell2),
	append(Cell2,Cell,Out).    
	
length_(Length, List) :- length(List, Length).

initarr(_,[]).
initarr(Value,[Row|Rest]):-
	init(Value,Row),
	initarr(Value,Rest).
	
init(_,[]):-
	!.%,writeln('base case').
init(Value,[Head|Tail]):-
	Head = Value,
	% write('Value is :'),writeln(Value),
	init(Value,Tail).
		
		
	
	
	
	
	
	
	
	
	
	