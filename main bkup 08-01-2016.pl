/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   N Queens animation.

   Written Feb. 2008 by Markus Triska (triska@gmx.at)
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpfd)).
:- use_module('analyze.pl').

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


play:-
% Board=[(4,4,'b'),(4,5,'r'),(5,4,'r'),(5,5,'b')],write('Board is: '),writeln(Board),
% check(Board,(2,3,'w')),
% check(Board,(3,X,Y)).
	retractall(x(X,Y)),
	retractall(o(X,Y)),
	check2(2,3),
	check2(4,5),
	check2(4,4),
	% Dim = 8,			% defines board size - here 8 x 8
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
	  % nl, write('Token for human player ? (x or o)'), nl,
	  % read(Player), nl,
	  Player = x,
	  (
	    Player \= o, Player \= x, !,     % If not x or o -> not a valid color
	    write('Error : not a valid color !'), nl,
	    playAskToken                     % Ask again
	    ;
		Dim = 8,
		showarr(Dim),
		Turn = 'x',
		getvalidmoves(Turn,Moves,ValidMoves), 
				% findall(Output,analyze(Turn,Moves,Output,0),Moves2),
				writef(' List of Potential moves for %w: %w\n',[Turn,Moves]),
				writef(' List of Valid moves for %w: %w\n',[Turn,ValidMoves]),
		getuserinput(Moves,NewLoc),
		writef('play_ask_token: New location is %w \n',[NewLoc]),
		add_newgain(Player,NewLoc,[],GainedList),
		writef('play_ask_token: List of cells gained is now: %w\n',[GainedList]),	
		writef('play_ask_token: assert gains for %w\n',[Turn]),
		assert_gains(Turn,GainedList),
		showarr(Dim),	
		!
	    % Start the game with color and emptyBoard
	    % play([x, play, EmptyBoard], Player)
	  ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   add new cell gained by player +Turn at +Loc 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */	  
	  
add_newgain(_,[],_,_):-writeln('Base case').
add_newgain(Turn,Loc,GainedList,[Loc|GainedList]):-

	[X,Y]=Loc,
	NewFact=..[Turn,X,Y],
	writef(' add_newgain: Non-Base case . NewFact is %w \n',[NewFact]),
	assertz(NewFact),check2(5,5),
	showarr(8),	
	% append( 		[[Loc]],GainedList,NewGainedList),
	writef(' add_newgain: List of cells gained is now: %w\n',[Loc|GainedList]).

	
assert_gains(_,[]):-writeln('assert_gains: Base case - Empty gained list').		  

assert_gains(Player,[[Row,Col]|RestGainedList]):-
	opp_player(Player,Opp_Player),
		writef('assert_gains(1): for %w, centered around %w,%w OppPlayer: %w\n',[Player,Row,Col,Opp_Player]),
	findadj_disks2([Row,Col],Opp_Player,OppList),
		writef("assert_gains(1): Cells with opponent token %w adjacent to gained cell: %w/%w : %w\n",[Opp_Player,Row,Col,OppList]),
	eval_gains(Opp_Player,OppList,RestGainedList,NewGainedList),
		writef('assert_gains(2): Rest Gained List is:   %w \n',[RestGainedList]),
		writef('assert_gains(2):  New Gained List is:   %w \n',[NewGainedList]),
	append(RestGainedList,NewGainedList,RestGainedList2),
		writef('assert_gains(2):  Rest Gained List2 is:   %w \n',[RestGainedList2]),
	assert_gains(Player,RestGainedList2),
	true.

eval_gains(_,[[]],_,_):-writeln('eval_gains: base case'). 	

eval_gains(Opp_Player,[OppCell|RestOppList],GainedList,NewGainedList):- 
	[X,Y,Dir] = OppCell,
	writef("eval_gains(2): Evaluate cell chain containing opponent token %w @ Row/Col - Dir: [%w,%w] - %w  \n",[Opp_Player,X,Y,Dir]),
	writef("               RestOppList is %w \n",[RestOppList]),
	process_cell_chain(Opp_Player,[X,Y],Dir,Score,OutScore,TknFound,GainedList,NewGainedList),

    writef('eval_gains(1):     RestOppList is:   %w \n',[RestOppList]),
    writef('eval_gains(1):     Gained List is:   %w \n',[GainedList]),
	writef('eval_gains(1): Returned Gained List is:   %w \n',[NewGainedList]),
	append(GainedList,NewGainedList,GainedList2),
    writef('eval_gains(1):  NewGained List is:   %w \n',[GainedList2]),
	
	eval_gains(Opp_Player,[RestOppList],GainedList2,NewGainedList2),
	
	writef('eval_gains(2):      Gained List is:   %w \n',[GainedList]),
	writef('eval_gains(2):      Gained List is:   %w \n',[GainedList2]),
	writef('eval_gains(2):  New Gained List is:   %w \n',[NewGainedList]),
	% writef('eval_gains(2): Result GainedList is:   %w \n',[ResultGainedList]),
	true.

/*--------------------------------------------------------------------------   
	Process Cell Chain
--------------------------------------------------------------------------*/		
	
process_cell_chain(Opp_Player,[X,Y],Dir,Score,0,0,_,_):-
	get_next_cell(Dir,[X,Y],[X2,Y2]),
	between(1,8,X2),between(1,8,Y2),
	writef('     a- Determine if next cell is (empty) for player %w at location: %w/%w - %w Score is: %w \n',[Opp_Player,X2,Y2,Dir,Score]),
	free(X2,Y2), 
	writef('        Location: %w/%w  is empty \n',[X2,Y2]),
	!.

process_cell_chain(Opp_Player,[X,Y],Dir,Score,OutScore,TknFound,GainedList,NewGainedList):-
	get_next_cell(Dir,[X,Y],[X2,Y2]),
	between(1,8,X2),between(1,8,Y2),
	writef('     b- Determine if next_cell is same tkn as [%w] at location: %w/%w - %w Score is: %w\n',[Opp_Player,X2,Y2,Dir,Score]),
	call(Opp_Player,X2,Y2),
	writef('     b- Location: %w/%w  has same token\n',[X2,Y2]),
	process_cell_chain(Opp_Player,[X2,Y2],Dir,Score2,OutScore,TknFound,MoreGainedList2),
	writef('     b- Return from recursive chain x2/y2: %w,%w Score/Outscore:%w/%w TknFound: %w\n',[X2,Y2,Score,OutScore,TknFound]),
	((TknFound =:= 1) -> (opp_player(Player,Opp_Player), add_newgain(Player,[X,Y],GainedList,MoreGainedList2))),
	writef('     b- opp token (%w) found at x2/y2: %w,%w Score: %w  Outscore:%w TknFound: %w\n',[Player,X2,Y2,Score,OutScore,TknFound]),
	!.

process_cell_chain(Opp_Player,[X,Y],Dir,Score,OutScore,TknFound,GainedList,NewGainedList):-
	get_next_cell(Dir,[X,Y],[X2,Y2]),
	between(1,8,X2),between(1,8,Y2),
	writef('     c- Determine if next cell has opp token of [%w] at location: %w/%w - %w Score is: %w\n',[Opp_Player,X2,Y2,Dir,Score]),
	opp_player(Player,Opp_Player),
	call(Player,X2,Y2),
	TknFound = 1, Score is 1, OutScore is 1,
	writef('     c- TknFound is: %w found at x2/y2: %w,%w Score: %w  Outscore:%w \n',[Player,X2,Y2,Score,OutScore]),
	((TknFound =:= 1) -> (opp_player(Player,Opp_Player), add_newgain(Player,[X,Y],GainedList,NewGainedList))),

	writef('     c- NewGained List is:   %w \n',[NewGainedList]),
	true.

get_next_cell(1,[X,Y],[X2,Y2]):-X2 is X-1, Y2 is Y-1.
get_next_cell(2,[X,Y],[X2,Y2]):-X2 is X-1, Y2 is Y.
get_next_cell(3,[X,Y],[X2,Y2]):-X2 is X-1, Y2 is Y+1.

get_next_cell(4,[X,Y],[X2,Y2]):-X2 is X  , Y2 is Y-1.
% get_next_cell(5,[X,Y],[X2,Y2]):-X2 is X-1, Y2 is Y.
get_next_cell(6,[X,Y],[X2,Y2]):-X2 is X  , Y2 is Y+1.

get_next_cell(7,[X,Y],[X2,Y2]):-X2 is X+1, Y2 is Y-1.
get_next_cell(8,[X,Y],[X2,Y2]):-X2 is X+1, Y2 is Y.
get_next_cell(9,[X,Y],[X2,Y2]):-X2 is X+1, Y2 is Y+1.
/*--------------------------------------------------------------------------   
  findadj_opp - Find cells of type +Player adjacent to [+Row,+Col] occupied by opponent 
  return in -List
  Uses constraint programming 
--------------------------------------------------------------------------*/	
findadj_disks([Row,Col],Player,Outlist):-
	% writef("Find avail spots around OO token at Row/Col is: %w/%w \n",[Row,Col]),
	Cell=[X,Y],
	Row_m1 is Row -1, Row_p1 is Row +1,
	Col_m1 is Col -1, Col_p1 is Col +1,
	X in Row_m1..Row_p1,
	Y in Col_m1..Col_p1,
	% (X #= Row) #==> (Y #\=Col),
	% (Y #= Col) #==> (X #\=Row),
	findall([X,Y],(call(Player,X,Y)),List),		% findall OO's surrounding Cell
	tuples_in([[X,Y]],List),						% must be a free cell
	findall(Cell,label(Cell),Outlist).
	
/*--------------------------------------------------------------------------   
  findadj_opp - Find cells of type +Player adjacent to [+Row,+Col] occupied by opponent 
  return in -List
  Uses constraint programming 
--------------------------------------------------------------------------*/	
findadj_disks2([Row,Col],Player,Outlist):-
	% writef("Find avail spots around OO token at Row/Col is: %w/%w \n",[Row,Col]),
	Cell=[X,Y,Dir],
	X in 1..8,
	Y in 1..8,
	Dir in 1..9,
	Row_m1 is Row -1, Row_p1 is Row +1,
	Col_m1 is Col -1, Col_p1 is Col +1,
	X in Row_m1..Row_p1,
	Y in Col_m1..Col_p1,
	Dir #= ((X - Row + 1) * 3) + (Y - Col + 2),
	Dir #\= 5,
	findall([X,Y],(call(Player,X,Y)),List),		% findall OO's surrounding Cell
	tuples_in([[X,Y]],List),						% must be a free cell
	findall(Cell,label(Cell),Outlist).
		
/*--------------------------------------------------------------------------   
  Determine all valid moves for player O
  Results placed in Moves, a list of locations [[X,Y],....] 
--------------------------------------------------------------------------*/
getuserinput(Moves,NewLoc):-
	write('Enter next move: Row,Col and press enter --> '),
	(read((R,C)) ->
		writef("Entered Loc is >%w< >%w<   Valid Moves: %w  \n",[R,C,Moves])
		;
		(writef('Invalid entry - Please try again \n'),read((R,C)) )
	),
	Loc = [R,C], 
	writef("Entered Loc is %w   Valid Moves: %w  \n",[Loc,Moves]),
	(memberchk(Loc,Moves) -> 
		(writef('%w is a valid move \n',[Loc]),NewLoc = [R,C])
		;
		(writef('%w is an invalid move - Please try again \n',[Loc]),getuserinput(Moves,NewLoc))
	),
nl.

/*--------------------------------------------------------------------------   
  Determine all valid moves for player O
  Results placed in Moves, a list of locations [[X,Y],....] 
  --------------------------------------------------------------------------*/
getvalidmoves(Turn,Moves,Output):-
	validmoves(Turn,Moves), 
	% writef('List of potential moves for %w: %w\n',[Turn,Moves]),
	analyze_moves(Turn,Moves,Output),
	% writef('List of possible moves for %w: %w\n',[Turn,Output]),
	nl.
	
validmoves(Turn,Moves):-
	opp_player(Turn,Opp_Player),
	findall([R,C],call(Opp_Player,R,C),Listx),
	findall(Res,(member(Elem,Listx),findadjfree(Elem,Res)),Moves_o),
	append(Moves_o,Moves),
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
 	Some extra functions
  	   display some information when required
 --------------------------------------------------------------------------*/	


free(X,Y):-
	\+ x(X,Y) , \+ o(X,Y).

opp_player('x','o').

opp_player('o','x').
	
disp(_,(R,C)):-
 writef('  Row is : '),write(R),write('  Col is: '),writeln(C).

check2(Row,Col):-
 o(Row,Col),writef('A O piece is at Row/Col: %w,%w \n',[Row,Col]),!.
 
check2(Row,Col):-
 x(Row,Col),writef('A X piece is at Row/Col: %w,%w \n',[Row,Col]),!.

check2(Row,Col):-
 writef('No piece is at Row/Col: %w,%w',[Row,Col]),!.

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
	% check2(5,5),
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


 