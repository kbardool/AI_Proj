
:- module(analyze,[analyze_horizontal/3,analyze_vertical/3,analyze_diagonal/3]).

	
/*--------------------------------------------------------------------------   
  Analyze Horizontial moves
 
--------------------------------------------------------------------------*/		
analyze_horizontal(Turn,[X,Y],OutScore):-
 	writef('  *-- Determine horizontal moves for player %w at location: %w/%w \n',[Turn,X,Y]),
	InScore is 0,
	writef('  call a_h_f \n'),
	a_h_f(Turn,X,Y,InScore,OutScoreHF,TknFound),
	writef('  call a_h_b \n'),
	a_h_b(Turn,X,Y,InScore,OutScoreHB),
	writef('  *-- 1 \n'),
	writef('  *-- a_h_f score: %w   a_h_b score : %w \n',[OutScoreHF,OutScoreHB]),
	writef('  *-- 2 \n'),
	OutScore  is OutScoreHF + OutScoreHB.

	
a_h_f(Turn,X,Y,0,0,TknFound):-
	Y < 8,
	Y2 is Y + 1,
	writef('     a- Determine forward horizontal (free) for player %w at location: %w/%w Score is: %w \n',[Turn,X,Y2,Score]),
	free(X,Y2), 
	writef('     a- Location: %w/%w  is empty\n',[X,Y2]),
	!.
	
a_h_f(Turn,X,Y,Score,Score,TknFound):- 
	Y < 8,
	Y2 is Y + 1,
	writef('     b- Determine forward horizontal moves(same tkn) for player %w at location: %w/%w Score is: %w\n',[Turn,X,Y2,Score]),
	% call(Turn,X,Y2),
	recorded(Turn,[X,Y2]), 
	TknFound = 1,
	writef('     b- Location: %w/%w  has same token\n',[X,Y2]),
	!.
	
a_h_f(Turn,X,Y,Score,OutScore,TknFound):-
    Y < 8,
	Y2 is Y + 1,
	writef('     c- Determine forward horizontal moves(opp token) for player %w at location: %w/%w Score is: %w\n',[Turn,X,Y2,Score]),
	opp_player(Turn,Opp_Player),
	
	% call(Opp_Player,X,Y2),
	recorded(Opp_Player,[X,Y2]),
		(recorded(Opp_Player,[X,Y2]) -> writef('Token %w found at %w,%w \n',[Opp_Player,X,Y2]) ; true),
	a_h_f(Turn,X,Y2,Score2,OutScore,TknFound),
	((TknFound =:= 1) -> (Score2 is OutScore + 1) ; true),
	writef('     c- return from a_h_f with Y2: %w and Score: %w \n',[Y2,Score2]), 
	OutScore is Score2.
	
%% backwards search -----------------------------------------------------------------	
	
a_h_b(Turn,X,Y,0,0):- 
	Y > 1, 	Y2 is Y - 1,
	writef('     a- Determine backward horizontal (free) for player %w at location: %w/%w \n',[Turn,X,Y2]),
	free(X,Y2),
	writef('     a- Location is empty\n'),
	!.
	
a_h_b(Turn,X,Y,Score,Score):-
	Y > 1, 	Y2 is Y - 1,
	writef('     b- Determine backward horizontal moves(same tkn) for player %w at location: %w/%w \n',[Turn,X,Y2]),
	% call(Turn,X,Y2),
	recorded(Turn,[X,Y2]),
	writef('     b- Same token in this loc \n'),
	!.
	
a_h_b(Turn,X,Y,Score,OutScore):-
	Y > 1, 	Y2 is Y -1,
	writef('     c- Determine backward horizontal moves(opp token) for player %w at location: %w/%wScore is: %w\n',[Turn,X,Y2,Score]),
 	opp_player(Turn,Opp_Player),
	Score2 is Score + 1, 
	% call(Opp_Player,X,Y2),
	(recorded(Opp_Player,[X,Y2]) -> writef('Token %w found at %w,%w \n',[Opp_Player,X,Y2]) ; true),
	a_h_b(Turn,X,Y2,Score2,OutScore),
	writef('     c- return from a_h_b with Y2: %w and Score: %w \n',[Y2,Score2]), 
	OutScore is Score2.

/*--------------------------------------------------------------------------   
  Analyze Vertical moves
 
--------------------------------------------------------------------------*/	
analyze_vertical(Turn,[X,Y],OutScore):-
 	writef('  *-- Determine Vertical moves for player %w at location: %w/%w \n',[Turn,X,Y]),
	InScore is 0,
	% writef('  call a_v_f \n'),
	a_v_f(Turn,X,Y,InScore,OutScoreVF),
	% writef('  call a_v_b \n'),
	a_v_b(Turn,X,Y,InScore,OutScoreVB),
	writef('  *-- a_v_f score: %w   a_v_b score : %w \n',[OutScoreVF,OutScoreVB]),
	OutScore is OutScoreVF + OutScoreVB.

a_v_f(Turn,X,Y,_,0):-
	X < 8, 	X2 is X + 1,
	% writef('     a- Determine forward vertical (free) for player %w at location: %w/%w Score is: %w \n',[Turn,X2,Y,Score]),
	free(X2,Y), 
	% writef('        Location: %w/%w  is empty\n',[X2,Y]),
	!.
	
a_v_f(Turn,X,Y,Score,Score):- 
	X < 8,
	X2 is X + 1,
	% writef('     b- Determine forward vertical moves(same tkn) for player %w at location: %w/%w Score is: %w\n',[Turn,X2,Y,Score]),
	% call(Turn,X2,Y),
		recorded(Turn,[X2,Y]),
	% writef('        Location: %w/%w  has same token\n',[X2,Y]), 
	!.
 
a_v_f(Turn,X,Y,Score,OutScore):-
	X < 8,	X2 is X + 1,	
	% writef('     c- Determine forward vertical moves(opp token) for player %w at location: %w/%w Score is: %w\n',[Turn,X2,Y,Score]),
	opp_player(Turn,Opp_Player),
	Score2 is Score + 1, 
	% call(Opp_Player,X2,Y),
		recorded(Opp_Player,[X2,Y]),
		writef('     c- Token %w found at %w,%w  \n',[Opp_Player,X2,Y]), 
	a_v_f(Turn,X2,Y,Score2,OutScore).
	% writef('        return from a_v_f with x2: %w and Score: %w \n',[X2,Score2]).
	
%% backwards search -----------------------------------------------------------------	

a_v_b(Turn,X,Y,_,0):- 
	X > 1, 	X2 is X - 1,
	% writef('   a- Determine backward vertical (free) for player %w at location: %w/%w \n',[Turn,X2,Y]),
	free(X2,Y),
	% writef('      Location is empty\n'), !.
	!.
	
	
a_v_b(Turn,X,Y,Score,Score):-
	X > 1, 	X2 is X - 1,
	% writef('   b- Determine backward vertical moves(same tkn) for player %w at location: %w/%w \n',[Turn,X2,Y]),
	% call(Turn,X2,Y),
		recorded(Turn,[X2,Y]),
	% writef('      Same token in this loc \n'),!.	
	!.
	
	
a_v_b(Turn,X,Y,Score,OutScore):-
	X > 1, 	X2 is X -1,
	% writef('   c- Determine backward vertical moves(opp token) for player %w at location: %w/%wScore is: %w\n',[Turn,X2,Y,Score]),
 	opp_player(Turn,Opp_Player),
	Score2 is Score + 1, 
	% call(Opp_Player,X2,Y),
		recorded(Opp_Player,[X2,Y]),
	a_v_b(Turn,X2,Y,Score2,OutScore).
	% writef('     return from a_v_b with X2: %w and Score: %w \n',[X2,Score2]), 

/*--------------------------------------------------------------------------   
  Analyze Diagonal moves
 
--------------------------------------------------------------------------*/	
analyze_diagonal(Turn,[X,Y],OutScore):-
 	% writef('  *-- Determine diagonal moves for player %w at location: %w/%w \n',[Turn,X,Y]),
	InScore is 0,
	% writef('  call a_d_f \n'),
	a_d_f(Turn,X,Y,InScore,OutScoreDF),
	% writef('  call a_d_b \n'),
	a_d_b(Turn,X,Y,InScore,OutScoreDB),
	% writef('  *-- a_d_f score: %w   a_d_b score : %w \n',[OutScoreDF,OutScoreDB]),
	OutScore is OutScoreDF + OutScoreDB.

a_d_f(Turn,X,Y,_,0):-
	X < 8, X2 is X + 1,
	Y < 8, Y2 is Y + 1, 
	 % writef('     a- Determine forward diagonal (free) for player %w at location: %w/%w Score is: %w \n',[Turn,X2,Y2,Score]),
	free(X2,Y2), 
	% writef('        Location: %w/%w  is empty \n',[X2,Y2]),
	!.
	% true.
	
a_d_f(Turn,X,Y,Score,OutScore):- 
	X < 8, X2 is X + 1,
	Y < 8, Y2 is Y + 1,
	% Score =:= 1,
	% writef('     b- Determine forward diagonal moves(same tkn) for player %w at location: %w/%w Score is: %w\n',[Turn,X2,Y2,Score]),
	% call(Turn,X2,Y2),
		recorded(Turn,[X2,Y2]),
	OutScore is Score,
	% writef('        Location: %w/%w  has same token\n',[X2,Y2]),
	!.
 
a_d_f(Turn,X,Y,Score,OutScore):-
	X < 8, X2 is X + 1,
	Y < 8, Y2 is Y + 1, 
	% writef('     c- Determine forward diagonal moves(opp token) for player %w at location: %w/%w Score is: %w\n',[Turn,X2,Y2,Score]),
	opp_player(Turn,Opp_Player),
	Score2 is Score + 1, 
	% call(Opp_Player,X2,Y2),
		recorded(Opp_Player,[X2,Y2]),
	a_d_f(Turn,X2,Y2,Score2,OutScore2),
	OutScore is OutScore2,
	% writef('        return from a_d_f with x2/y2: %w,%w Score: %w  Outscore:%w \n',[X2,Y2,Score2,OutScore]),
	true.

%% backwards search -----------------------------------------------------------------

a_d_b(Turn,X,Y,_,0):- 
	X < 8, X2 is X - 1,
	Y < 8, Y2 is Y - 1, 
	% writef('   a- Determine backward diagonal (free) for player %w at location: %w/%w \n',[Turn,X2,Y2]),
	free(X2,Y2),
	% writef('      Location is empty\n'),
	!.
	
a_d_b(Turn,X,Y,Score,Score):-
	X < 8, X2 is X - 1,
	Y < 8, Y2 is Y - 1, 
	% writef('   b- Determine backward diagonal moves(same tkn) for player %w at location: %w/%w \n',[Turn,X2,Y2]),
	% call(Turn,X2,Y2),
		recorded(Turn,[X2,Y2]),
	% writef('      Same token in this loc \n'),
	!.	
 
a_d_b(Turn,X,Y,Score,OutScore):-
	X < 8, X2 is X - 1,
	Y < 8, Y2 is Y - 1, 
	% writef('   c- Determine backward diagonal moves(opp token) for player %w at location: %w/%wScore is: %w\n',[Turn,X2,Y2,Score]),
 	opp_player(Turn,Opp_Player),
	Score2 is Score + 1, 
	% call(Opp_Player,X2,Y2),
		recorded(Opp_Player,[X2,Y2]),
	a_d_b(Turn,X2,Y2,Score2,OutScore),
	% writef('     return from a_d_b with x2/y2: %w,%w and Score: %w \n',[X2,Y2,Score2]),
	true.	
 