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
/*  Positions stored as following facts
	x(R,C): X Token in Row R, Col C is 
	o(R,C): O Token in Row R, Col C
*/

% x(4,5).
% x(5,4).
% o(7,8).
% o(4,4).
% o(5,5).


test1:-
% Board=[(4,4,'b'),(4,5,'r'),(5,4,'r'),(5,5,'b')],write('Board is: '),writeln(Board),
% check(Board,(2,3,'w')),
% check(Board,(3,X,Y)).
	retractall(x(X,Y)),
	retractall(o(X,Y)),
	check2(2,3),
	check2(4,5),
	check2(4,4),
	Dim is 8,			% defines board size - here 8 x 8
	assert(x(4,5)), asserta(x(5,4)),asserta(o(4,4)),assert(o(5,5)),
	check2(2,3),
	check2(4,5),
	check2(4,4),
	% Turn = 'o',
	% findall((R,C),o(R,C),List),
	% writef('O in locations: %w \n',[List]),
% current_input(In),current_output(Out),
% writef(' Input stream is %w, Output stream is %w \n',[In,Out]),
    nl,
    write('===================='), nl,
	write('== Prolog Othello =='), nl,
	write('===================='), nl, nl,
	write('Reminder : x always starts the game'), nl,
	playAskToken.



playAskToken :-
	  nl, write('Token for human player ? (x or o)'), nl,
	  read(Player), nl,
	  (
	    Player \= o, Player \= x, !,     % If not x or o -> not a valid color
	    write('Error : not a valid color !'), nl,
	    playAskToken                     % Ask again
	    ;
		Dim = 8,
		showarr(Dim),
		Turn = 'x',
		getvalidmoves(Turn,Moves,ValidMoves), 
		% writef('1 List of valid moves for %w are: %w\n',[Turn,Moves]),
		% findall(Output,analyze(Turn,Moves,Output,0),Moves2),
		% writef(' List of Valid moves for %w: %w\n',[Turn,Moves]),
		writef(' List of Valid moves and weights for %w: %w\n',[Turn,ValidMoves]),
		getuserinput(Moves,NewLoc),
		writef(' New location is %w \n',[NewLoc]),
		putnewtoken(Turn,NewLoc),
		showarr(Dim),	
		!
	    % Start the game with color and emptyBoard
	    % play([x, play, EmptyBoard], Player)
	  ).

/*--------------------------------------------------------------------------   
  Determine all valid moves for player O
  Results placed in Moves, a list of locations [[X,Y],....] 
--------------------------------------------------------------------------*/
getuserinput(Moves,NewLoc):-
	write('Enter next move: Row,Col and press enter --> '),
	read((R,C)),
	Loc = [R,C], 
	writef("Entered Loc is %w   Loc2 is %w  Moves: %w  \n",[Loc,Moves]),
	(memberchk(Loc,Moves) -> 
		(writef('Valid move \n'),NewLoc = [R,C],writef(' New location is %w \n',[NewLoc]))
		;
		(writef('Invalid move %w - Please try again \n',[Loc]),getuserinput(Moves,NewLoc))
	),
nl.

/*--------------------------------------------------------------------------   
  Determine all valid moves for player O
  Results placed in Moves, a list of locations [[X,Y],....] 
  --------------------------------------------------------------------------*/
getvalidmoves(Turn,Moves,Output):-
	validmoves(Turn,Moves), 
	writef('List of potential moves for %w: %w\n',[Turn,Moves]),
    % findall(Output,analyze_moves(Turn,Moves,Moves_o),Output),
	analyze_moves(Turn,Moves,Output),
	writef('List of possible moves for %w: %w\n',[Turn,Output]),
	nl.
	
validmoves(Turn,Moves):-
	opp_player(Turn,Opp_Player),
	findall([R,C],call(Opp_Player,R,C),Listx),
	findall(Res,(member(Elem,Listx),findadjfree(Elem,Res)),Moves_o),
	append(Moves_o,Moves),
	writef("List of valid moves for %w is: %w \n",[Turn,Moves]),
	true.
	
/*--------------------------------------------------------------------------   
  findadjfree - Find available cells adjacent to [Row,Col]
  Uses constraint programming 
--------------------------------------------------------------------------*/	
findadjfree([Row,Col],Outlist):-
	% writef("Find avail spots around OO token at Row/Col is: %w/%w \n",[Row,Col]),
	Cell=[X,Y],
	Row_m1 is Row -1, Row_p1 is Row +1,
	Col_m1 is Col -1, Col_p1 is Col +1,
	X in Row_m1..Row_p1,
	Y in Col_m1..Col_p1,
	(X #= Row) #==> (Y #\=Col),
	(Y #= Col) #==> (X #\=Row),
	% free(X,Y),
	findall([X,Y],(o(X,Y);x(X,Y)),List),		% findall OO's surrounding Cell
	#\ tuples_in([[X,Y]],List),					% must be a free cell
	findall(Cell,label(Cell),Outlist).
	
 
/*--------------------------------------------------------------------------   
  Determine all valid moves for player O
  Results placed in Moves, a list of locations [[X,Y],....] 
--------------------------------------------------------------------------*/
analyze_moves(Turns,Moves,Output):-
	analyze_(Turns, Moves, [], Output).
		
analyze_(_,[],Temp,Temp).
	% writef('No more locations to analyze - Temp is %w \n\n',[Temp]).
	
analyze_(Turn,[Loc|Rest], Temp, Output):-
	[X,Y] = Loc,
	% Output = [X,Y,Score],
 	% writef('\n *** Determine valid moves for player %w at location: %w \n',[Turn,Loc]),

	analyze_horizontal(Turn,Loc,OutScoreH),
	analyze_vertical(Turn,Loc,OutScoreV),
	analyze_diagonal(Turn,Loc,OutScoreD),

	Score is OutScoreD + OutScoreH + OutScoreV,
	
	writef(' *** Loc: [%w,%w]   H-Score: %w   V-score: %w  D-score: %w  Total Score: %w\n',[X,Y,OutScoreH,OutScoreV,OutScoreD,Score]),
	% writef('Before Append - Temp is: %w  Temp2 is: %w \n',[Temp,Temp2]),		
	(Score > 0 -> 
		append([[X,Y,Score]],Temp,Temp2)
		; 
		(Temp2 = Temp)
	),
	% writef('After Append - Temp is: %w  Temp2 is: %w \n',[Temp,Temp2]),		

	analyze_(Turn,Rest,Temp2,Output),
    % writef('Output is:   %w \n',[Output]),
	% writef('Temp2  is:   %w \n',[Temp2]),		
	true.
	
	
/*--------------------------------------------------------------------------   
  Analyze Horizontial moves
 
--------------------------------------------------------------------------*/		
analyze_horizontal(Turn,[X,Y],OutScore):-
 	% writef('  *-- Determine horizontal moves for player %w at location: %w/%w \n',[Turn,X,Y]),
	InScore is 0,
	% writef('  call a_h_f \n'),
	a_h_f(Turn,X,Y,InScore,OutScoreHF),
	% writef('  call a_h_b \n'),
	a_h_b(Turn,X,Y,InScore,OutScoreHB),
	% writef('  *-- a_h_f score: %w   a_h_b score : %w \n',[OutScoreHF,OutScoreHB]),
	OutScore  is OutScoreHF + OutScoreHB.

a_h_f(Turn,X,Y,Score,Score):-
	Y < 8,
	Y2 is Y + 1,
	% writef('     a- Determine forward horizontal (free) for player %w at location: %w/%w Score is: %w \n',[Turn,X,Y2,Score]),
	free(X,Y2), 
	% writef('        Location: %w/%w  is empty\n',[X,Y2]), !.
	!.
a_h_f(Turn,X,Y,Score,Score):- 
	Y < 8,
	Y2 is Y + 1,
	% writef('    b- Determine forward horizontal moves(same tkn) for player %w at location: %w/%w Score is: %w\n',[Turn,X,Y2,Score]),
	call(Turn,X,Y2),
	% writef('        Location: %w/%w  has same token\n',[X,Y2]), !.
	!.
a_h_f(Turn,X,Y,Score,OutScore):-
    Y < 8,
	Y2 is Y + 1,
	% writef('     c- Determine forward horizontal moves(opp token) for player %w at location: %w/%w Score is: %w\n',[Turn,X,Y2,Score]),
	opp_player(Turn,Opp_Player),
	Score2 is Score + 1, 
	call(Opp_Player,X,Y2),
	a_h_f(Turn,X,Y2,Score2,OutScore),
	% writef('        return from a_h_f with Y2: %w and Score: %w \n',[Y2,Score2]), 
	OutScore is Score2.
	
a_h_b(Turn,X,Y,Score,Score):- 
	Y > 1, 	Y2 is Y - 1,
	% writef('     a- Determine backward horizontal (free) for player %w at location: %w/%w \n',[Turn,X,Y2]),
	free(X,Y2),
	% writef('        Location is empty\n'), !.
	!.
a_h_b(Turn,X,Y,Score,Score):-
	Y > 1, 	Y2 is Y - 1,
	% writef('     b- Determine backward horizontal moves(same tkn) for player %w at location: %w/%w \n',[Turn,X,Y2]),
	call(Turn,X,Y2),
	% writef('        Same token in this loc \n'),!.	
	!.
a_h_b(Turn,X,Y,Score,OutScore):-
	Y > 1, 	Y2 is Y -1,
	% writef('     c- Determine backward horizontal moves(opp token) for player %w at location: %w/%wScore is: %w\n',[Turn,X,Y2,Score]),
 	opp_player(Turn,Opp_Player),
	Score2 is Score + 1, 
	call(Opp_Player,X,Y2),
	a_h_b(Turn,X,Y2,Score2,OutScore),
	% writef('       return from a_h_b with Y2: %w and Score: %w \n',[Y2,Score2]), 
	OutScore is Score2.

/*--------------------------------------------------------------------------   
  Analyze Vertical moves
 
--------------------------------------------------------------------------*/	
analyze_vertical(Turn,[X,Y],OutScore):-
 	% writef('  *-- Determine Vertical moves for player %w at location: %w/%w \n',[Turn,X,Y]),
	InScore is 0,
	% writef('  call a_v_f \n'),
	a_v_f(Turn,X,Y,InScore,OutScoreVF),
	% writef('  call a_v_b \n'),
	a_v_b(Turn,X,Y,InScore,OutScoreVB),
	% writef('  *-- a_v_f score: %w   a_v_b score : %w \n',[OutScoreVF,OutScoreVB]),
	OutScore is OutScoreVF + OutScoreVB.

a_v_f(Turn,X,Y,Score,0):-
	X < 8, 	X2 is X + 1,
	% writef('     a- Determine forward vertical (free) for player %w at location: %w/%w Score is: %w \n',[Turn,X2,Y,Score]),
	free(X2,Y), 
	% writef('        Location: %w/%w  is empty\n',[X2,Y]),
	!.
	
a_v_f(Turn,X,Y,Score,Score):- 
	X < 8,
	X2 is X + 1,
	% writef('     b- Determine forward vertical moves(same tkn) for player %w at location: %w/%w Score is: %w\n',[Turn,X2,Y,Score]),
	call(Turn,X2,Y),
	% writef('        Location: %w/%w  has same token\n',[X2,Y]), 
	!.
 
a_v_f(Turn,X,Y,Score,OutScore):-
	X < 8,	X2 is X + 1,	
	% writef('     c- Determine forward vertical moves(opp token) for player %w at location: %w/%w Score is: %w\n',[Turn,X2,Y,Score]),
	opp_player(Turn,Opp_Player),
	Score2 is Score + 1, 
	call(Opp_Player,X2,Y),
	a_v_f(Turn,X2,Y,Score2,OutScore).
	% writef('        return from a_v_f with x2: %w and Score: %w \n',[X2,Score2]).
	

a_v_b(Turn,X,Y,_,0):- 
	X > 1, 	X2 is X - 1,
	% writef('   a- Determine backward vertical (free) for player %w at location: %w/%w \n',[Turn,X2,Y]),
	free(X2,Y),
	% writef('      Location is empty\n'), !.
	!.
a_v_b(Turn,X,Y,Score,Score):-
	X > 1, 	X2 is X - 1,
	% writef('   b- Determine backward vertical moves(same tkn) for player %w at location: %w/%w \n',[Turn,X2,Y]),
	call(Turn,X2,Y),
	% writef('      Same token in this loc \n'),!.	
	!.
a_v_b(Turn,X,Y,Score,OutScore):-
	X > 1, 	X2 is X -1,
	% writef('   c- Determine backward vertical moves(opp token) for player %w at location: %w/%wScore is: %w\n',[Turn,X2,Y,Score]),
 	opp_player(Turn,Opp_Player),
	Score2 is Score + 1, 
	call(Opp_Player,X2,Y),
	a_v_b(Turn,X2,Y,Score2,OutScore).
	% writef('     return from a_v_b with X2: %w and Score: %w \n',[X2,Score2]), 

/*--------------------------------------------------------------------------   
  Analyze Diagonal moves
 
--------------------------------------------------------------------------*/	
analyze_diagonal(Turn,[X,Y],OutScore):-
 	writef('  *-- Determine diagonal moves for player %w at location: %w/%w \n',[Turn,X,Y]),
	InScore is 0,
	% writef('  call a_d_f \n'),
	a_d_f(Turn,X,Y,InScore,OutScoreDF),
	% writef('  call a_d_b \n'),
	a_d_b(Turn,X,Y,InScore,OutScoreDB),
	writef('  *-- a_d_f score: %w   a_d_b score : %w \n',[OutScoreDF,OutScoreDB]),
	OutScore is OutScoreDF + OutScoreDB.

a_d_f(Turn,X,Y,Score,0):-
	X < 8, X2 is X + 1,
	Y < 8, Y2 is Y + 1, 
	 writef('     a- Determine forward diagonal (free) for player %w at location: %w/%w Score is: %w \n',[Turn,X2,Y2,Score]),
	free(X2,Y2), 
	writef('        Location: %w/%w  is empty \n',[X2,Y2]),
	!.
	% true.
	
a_d_f(Turn,X,Y,Score,OutScore):- 
	X < 8, X2 is X + 1,
	Y < 8, Y2 is Y + 1,
	% Score =:= 1,
	writef('     b- Determine forward diagonal moves(same tkn) for player %w at location: %w/%w Score is: %w\n',[Turn,X2,Y2,Score]),
	call(Turn,X2,Y2),
	OutScore is Score,
	writef('        Location: %w/%w  has same token\n',[X2,Y2]),
	!.
 
a_d_f(Turn,X,Y,Score,OutScore):-
	X < 8, X2 is X + 1,
	Y < 8, Y2 is Y + 1, 
	writef('     c- Determine forward diagonal moves(opp token) for player %w at location: %w/%w Score is: %w\n',[Turn,X2,Y2,Score]),
	opp_player(Turn,Opp_Player),
	Score2 is Score + 1, 
	call(Opp_Player,X2,Y2),
	a_d_f(Turn,X2,Y2,Score2,OutScore2),
	OutScore is OutScore2,
	writef('        return from a_d_f with x2/y2: %w,%w Score: %w  Outscore:%w \n',[X2,Y2,Score2,OutScore]),
	true.





	
%% backwards search -----------------------------------------------------------------

a_d_b(Turn,X,Y,_,0):- 
	X < 8, X2 is X - 1,
	Y < 8, Y2 is Y - 1, 
	writef('   a- Determine backward diagonal (free) for player %w at location: %w/%w \n',[Turn,X2,Y2]),
	free(X2,Y2),
	writef('      Location is empty\n'),
	!.
	
a_d_b(Turn,X,Y,Score,Score):-
	X < 8, X2 is X - 1,
	Y < 8, Y2 is Y - 1, 
	writef('   b- Determine backward diagonal moves(same tkn) for player %w at location: %w/%w \n',[Turn,X2,Y2]),
	call(Turn,X2,Y2),
	writef('      Same token in this loc \n'),
	!.	
 
a_d_b(Turn,X,Y,Score,OutScore):-
	X < 8, X2 is X - 1,
	Y < 8, Y2 is Y - 1, 
	writef('   c- Determine backward diagonal moves(opp token) for player %w at location: %w/%wScore is: %w\n',[Turn,X2,Y2,Score]),
 	opp_player(Turn,Opp_Player),
	Score2 is Score + 1, 
	call(Opp_Player,X2,Y2),
	a_d_b(Turn,X2,Y2,Score2,OutScore),
	writef('     return from a_d_b with x2/y2: %w,%w and Score: %w \n',[X2,Y2,Score2]),
	true.	


/*--------------------------------------------------------------------------
 	Some extra functions
  	   display some information when required
 --------------------------------------------------------------------------*/	
putnewtoken(Turn,[]):-writeln('Base case').
putnewtoken(Turn,[X,Y]):-
	NewFact=..[Turn,X,Y],
	assertz(NewFact).
	

free(X,Y):-
	\+ x(X,Y) , \+ o(X,Y).

opp_player('x','o').

opp_player('o','x').
	
disp(_,(R,C)):-
 writef('  Row is : '),write(R),write('  Col is: '),writeln(C).

check2(Row,Col):-
 o(Row,Col),write('A O piece is at Row:'),write(Row),write(' Col: '),writeln(Col),!.
 
check2(Row,Col):-
 x(Row,Col),write('A X piece is at Row:'),write(Row),write(' Col: '),writeln(Col),!.

check2(Row,Col):-
 write('No piece at Row:'),write(Row),write(' Col: '),writeln(Col).

check(List,Element):-
member(Element,List),
write(Element),writeln(' is in List').

check(List,Element):-
\+ member(Element,List),
write(Element),writeln(' is NOT in List').

/*-------------------------------------------------------------------------- 
  Predicates used in displaying the table 
--------------------------------------------------------------------------*/
 showarr(Dim):-
	nl,nl,
	showheader(Dim,0),
	showtable(Dim,0).

showheader(Dim,Dim):-nl.
showheader(Dim,P):-
	P < Dim,	
    ( (P =:= 0, write('      |') ); (P =\= 0)),
	P2 is P + 1,
	writef("  %w  ",[P2]), 
	showheader(Dim,P2).
	
showtable(Dim,Dim):-	
	writef('      -%r \n',['-----',Dim]).
showtable(Dim,Row):-
	% writef("Show table, Row/Col is: %w/%w \n",[Row,Col]),
	Row < Dim ,
	writef('      -%r \n',['-----',Dim]),
	Row2 is Row + 1,
	write('L:'),write(Row2), write('   |'),
	showrow(Dim,Row2 ,0),	
	showtable(Dim,Row2).
	
	
showrow(Dim,_,Dim):-	nl.
showrow(Dim,Row,Col):-
 	% writef("showrow, Dim/Row/Col is: %w/%w/%w \n",[Dim,Row,Col]),
	% check2(6,3),
	Col < Dim,
	Col2 is Col + 1,
	getdisptoken(Row,Col2,Res),
	write(' '),write(Res),write(' |'),
	% ((Pos > 1, write('|'));(Pos == 1)),
	showrow(Dim, Row, Col2).

 getdisptoken(Row,Col,'OO'):-
 	% writef("getdisptoken(O), Row/Col is: %w/%w  ",[Row,Col]),
	% (o(Row,Col)->writeln('Yes');writeln('No')),!.
	o(Row,Col),!.
 getdisptoken(Row,Col,'XX'):-
 	% writef("getdisptoken(X), Row/Col is: %w/%w \n",[Row,Col]),
	% (x(Row,Col)->writeln('Yes');writeln('No')),!.
	x(Row,Col),!.
 getdisptoken(_,_,'  ').
