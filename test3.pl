/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   N Queens animation.

   Written Feb. 2008 by Markus Triska (triska@gmx.at)
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpfd)).
% :- include('show.pl').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Constraint posting.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
% A=[[a1,b1,c1,d1,e1,f1,g1,h1],[a2,b2,c2,d2,e2,f2,g2,h2],[a3,b3,c3,d3,e3,f3,g3,h3],[a4,b4,c4,d4,e4,f4,g4,h4],
   % [a5,b5,c5,d5,e5,f5,g5,h5],[a6,b6,c6,d6,e6,f6,g6,h6],[a7,b7,c7,d7,e7,f7,g7,h7],[a8,b8,c8,d8,e8,f8,g8,h8]],

test3:-
	Dim is 8,
	Turn = 'x',	
	findall([X,Y],o(X,Y),Listo),
	writef("List of O Locations: %w \n",[Listo]),
	% Loc=[4,4],
	findall(Res,(member(Elem,Listo),findadjfree('o',Elem,Res)),Listf),
	% findadjfree('o',Loc,Listf), 	
	writef("Outlist  is: %w \n",[Listf]),
	append(Listf,Validmoves),
	writef("Outlist  is: %w \n",[Listff]),
	%maplist(writeln, Out),
	% append([Vars], Vs), 
	% writef("Vs  is: %w \n",[Vs]),
	% label(Vs), 
	writeln('Story over').

	% labeling([],Outlist),
	   % write('outlist is '),writeln(Outlist).
 
/* check2(2,3),
check2(4,5),
check2(4,4),
Turn = 'blue', writeln(Turn),
findall((R,C),o(R,C),List),
write('Red in locations: '),writeln(List),
maplist(disp(1),List),
 */
findmoves_X(Moves):-
	writef("Find all possible moves for XX \n"),
	findall([X,Y],o(X,Y),ListofReds).
	
	
	
	
	
	
findadjfree('o',[Row,Col],Outlist):-
	writef("Find avail spots around OO token at Row/Col is: %w/%w \n",[Row,Col]),
	Cell=[X,Y],
	Row_m1 is Row -1, Row_p1 is Row +1,
	Col_m1 is Col -1, Col_p1 is Col +1,
	X in Row_m1..Row_p1,
	Y in Col_m1..Col_p1,
	% writef('X between %w and %w \n',[Row_m1, Row_p1]),
	% writef('Y between %w and %w \n',[Col_m1, Col_p1]),
	% X #>= Row_m1 , X #=< Row_p1, 
	(X #= Row) #==> (Y #\=Col),
	(Y #= Col) #==> (X #\=Row),
	% Y #>= Col_m1 , Y #=< Col_p1, 
	% free(X,Y),
	% writeln('check free'),
	findall([X,Y],o(X,Y),Listo),		% findall OO's surrounding Cell
	findall([X,Y],x(X,Y),Listx),	     
	append(Listo,Listx,List),
	#\ tuples_in([[X,Y]],List),
	findall(Cell,label(Cell),Outlist).
	
 
 
findadjfree('x',Outlist,(Row,Col),Outlist):-
	writef("Find avail spots around BLUE token at Row/Col is: %w/%w \n",[Row,Col]),
	% Vars=[X,Y],
	Row_m1 is Row -1, Row_p1 is Row +1,
	Col_m1 is Col -1, Col_p1 is Col +1,
	X in Row_m1..Row_p1,
	Y in Col_m1..Col_p1,
	% writef('X between %w and %w \n',[Row_m1, Row_p1]),
	% writef('Y between %w and %w \n',[Col_m1, Col_p1]),
	% X #>= Row_m1 , X #=< Row_p1, 
	(X #= Row) #==> (Y #\=Col),
	(Y #= Col) #==> (X #\=Row),
	% Y #>= Col_m1 , Y #=< Col_p1, 
	% free(X,Y),
	% writeln('check free'),
	findall([X,Y],o(X,Y),Listo),		% findall OO's surrounding Cell
	findall([X,Y],x(X,Y),Listx),	     
	append(Listo,Listx,List),
	#\ tuples_in([[X,Y]],List),
	findall([X,Y],label([X,Y]),Outlist).
	
	
validmoves1('o'):-
   writeln('Determine valid moves for OO'),
   findall((R,C),x(R,C),List),
   maplist(findadjfree('x',Outlist),List).

validmoves1('x'):-
   writeln('Determine valid moves for XX'	),
   findall((R,C),o(R,C),List),
   maplist(findadjfree('o',Outlist),List).

disp(_,(R,C)):-
 writef('  Row is : '),write(R),write('  Col is: '),writeln(C).

free(X,Y 	):- 
	writeln('check free'),
	findall([X,Y],o(X,Y),Listo),
	findall([X,Y],x(X,Y),Listx),
	append(Listo,Listx,List),
	write('lost is '),writeln(List),
	tuples_in([[X,Y]],List).
	
 

 
x(4,5).
x(5,4).
% o(7,8).
o(4,4).
o(5,5).