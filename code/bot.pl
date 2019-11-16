:- ensure_loaded('game_logic.pl').
:- ensure_loaded('game_model.pl').
:- ensure_loaded('graph.pl').
:- ensure_loaded('gameover.pl').
:- ensure_loaded('display.pl').

% init_game_state([[
%              [0, 0, 0, 0, 0, 0, 2, 0],
%              [0, 0, 0, 0, 2, 2, 0, 2],
%              [1, 1, 1, 2, 0, 1, 2, 0],
%              [0, 0, 0, 0, 0, 0, 0, 0],
%              [0, 0, 0, 0, 0, 0, 0, 0],
%              [0, 0, 0, 0, 0, 0, 0, 0],
%              [0, 0, 0, 0, 0, 0, 0, 0],
%              [0, 0, 0, 0, 0, 0, 0, 0]
%             ],
%             [
%              [0, 1, 1, 1, 1, 1, 1, 1, 0],
%              [2, 0, 0, 0, 0, 0, 2, 2, 2],
%              [2, 0, 0, 0, 2, 2, 2, 2, 2],
%              [2, 0, 0, 0, 0, 0, 0, 0, 2],
%              [2, 0, 0, 0, 0, 0, 0, 0, 2],
%              [2, 0, 0, 0, 0, 0, 0, 0, 2],
%              [2, 0, 0, 0, 0, 0, 0, 0, 2],
%              [2, 0, 0, 0, 0, 0, 0, 0, 2],
%              [0, 1, 1, 1, 1, 1, 1, 1, 0]
%             ],
%             8,
%             8,
%             1,
%             1,
%             1,
%             1-0 ]).

init_game_state([[
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [1, 0, 0, 0, 0, 0, 0, 0],
             [1, 2, 0, 0, 0, 0, 0, 0],
             [0, 0, 2, 0, 0, 1, 0, 0]
            ],
            [
             [0, 1, 1, 1, 1, 1, 1, 1, 0],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 2, 0, 0, 0, 0, 0, 2],
             [0, 1, 1, 1, 1, 1, 1, 1, 0]
            ],
            8,
            8,
            1,
            1,
            2,
            1-0 ]).



random_move(GameState, Move) :-
    valid_moves(GameState, ListOfMoves),
    random_member(Move, ListOfMoves).


% Level-2 Bot

greedy_move(GameState, BestMove) :-
    valid_moves(GameState,ListOfMoves),
    findall(Value-Move, (member(Move, ListOfMoves),move(Move, GameState, NewGameState),print(Move),nl, value(NewGameState, Value)), Result),
    keysort(Result,SortedResultAsc),
    reverse(SortedResultAsc,SortedResultDsc),
    print(SortedResultDsc),nl,
    %nth0(0,SortedResultDsc,BestValue-BestMove),
    group_pairs_by_key(SortedResultDsc, [_-BestMoves|_]),
    print(BestMoves),nl,
    length(BestMoves,Length),
    print(Length),nl,
    random_between(1,Length,Rnd),
    print(Rnd),nl,
    nth1(Rnd,BestMoves,BestMove).


value(GameState, Value) :- !,

    % print(GameState),
    GameState = [OctagonBoard,SquareBoard, Height, Width,_,_,_, NumPlays-CutFlag | []],
    get_game_previous_player(GameState,PrevPlayer),
    orient_board(OctagonBoard, SquareBoard,PrevPlayer,OrientedOctagonBoard,OrientedSquareBoard),
    build_graph([OrientedOctagonBoard, OrientedSquareBoard, Height, Width], PrevPlayer, Graph1),!,
    
    % Get SBValue1
    get_highest_sub_board_value(OrientedOctagonBoard,Width,Height,PrevPlayer,Graph1,SBValue1),

    % Get SBValue2
    remove_cuttable_squares(OrientedOctagonBoard,OrientedSquareBoard,PrevPlayer,NewSquareBoard),

    build_graph([OrientedOctagonBoard, NewSquareBoard, Height, Width], PrevPlayer, Graph2),!,
    get_highest_sub_board_value(OrientedOctagonBoard,Width,Height,PrevPlayer,Graph2,SBValue2),

    % Player 2 section



    % GameState2 = [OctagonBoard,SquareBoard, Height, Width,_,_,PrevPlayer, NumPlays-CutFlag | []],
    % valid_moves(GameState,ListOfMoves),
    % findall(Val-Move, (member(Move, ListOfMoves),move(Move, GameState2, NewGameState), value_simple(NewGameState, Val)), Result),
    % keysort(Result,SortedResultAsc),
    % reverse(SortedResultAsc,SortedResultDsc),
    % % print(SortedResultDsc),
    % nth0(0,SortedResultDsc,Value2-_),

    % write('-- Player('),write(PrevPlayer),write(') SBValue1: '),write(SBValue1), write(' SBValue2: '),write(SBValue2),write(' Value2: '),write(Value2),nl,
    % write('-- Player('),write(PrevPlayer),write(') SBValue1: '),write(SBValue1), write(' SBValue2: '),write(SBValue2),nl,

    Value is SBValue1 + SBValue2 .

value_simple(GameState, Value) :- !,

    % print(GameState),
    GameState = [OctagonBoard,SquareBoard, Height, Width,_,_,_, NumPlays-_ | []],
    get_game_previous_player(GameState,PrevPlayer),
    orient_board(OctagonBoard, SquareBoard,PrevPlayer,OrientedOctagonBoard,OrientedSquareBoard),
    build_graph([OrientedOctagonBoard, OrientedSquareBoard, Height, Width], PrevPlayer, Graph1),!,
    
    % Get SBValue1
    get_highest_sub_board_value(OrientedOctagonBoard,Width,Height,PrevPlayer,Graph1,SBValue1),

    % Get SBValue2
    remove_cuttable_squares(OrientedOctagonBoard,OrientedSquareBoard,PrevPlayer,NewSquareBoard),

    build_graph([OrientedOctagonBoard, NewSquareBoard, Height, Width], PrevPlayer, Graph2),!,
    get_highest_sub_board_value(OrientedOctagonBoard,Width,Height,PrevPlayer,Graph2,SBValue2),
    Value is SBValue1 + SBValue2.



get_highest_sub_board_value(OctagonBoard,Width,Height,Player,Graph,Value) :- !,get_highest_sub_board_value_iter(OctagonBoard,Width,Height,Player,Graph,Height,Value).

get_highest_sub_board_value_iter(_,_,_,_,[],_,1) :-!.
get_highest_sub_board_value_iter(_,_,_,_,_,0,0) :-!.

get_highest_sub_board_value_iter(OctagonBoard,Width,Height,Player,Graph,CurrentBoardSize,Value) :-

    AlternativeCount is Height - CurrentBoardSize + 1,

    % write('-sub_board_iter1 - Number of alternatives: '), write(AlternativeCount), nl,
    \+check_sub_boards(OctagonBoard,Width,Height,Player,Graph,AlternativeCount,AlternativeCount),

    NewSize is CurrentBoardSize - 1,
    get_highest_sub_board_value_iter(OctagonBoard,Width,Height,Player,Graph,NewSize,Value).

get_highest_sub_board_value_iter(OctagonBoard,Width,Height,Player,Graph,CurrentBoardSize,Value) :-

    AlternativeCount is Height - CurrentBoardSize + 1,

    % write('-sub_board_iter2 - Number of alternatives: '), write(AlternativeCount), nl,
    check_sub_boards(OctagonBoard,Width,Height,Player,Graph,AlternativeCount,AlternativeCount),

    Value is CurrentBoardSize.

check_sub_boards(_,_,_,_,_,_,0) :- !,fail.

check_sub_boards(OctagonBoard,Width,Height,Player,Graph,AlternativeCount,CurrentAlternative) :-
    LowBar is CurrentAlternative - 1,
    HighBar is LowBar + Height - AlternativeCount,
    % write('-- board_iter1 - Number of alternatives: '), write(AlternativeCount), write(' Lowbar: '),write(LowBar), write(' Highbar: '),write(HighBar),nl,
    
    \+get_valid_starters(OctagonBoard,Player,LowBar,Width,_),!,

    CurrentAlternative1 is CurrentAlternative - 1, 
    check_sub_boards(OctagonBoard,Width,Height,Player,Graph,AlternativeCount,CurrentAlternative1).


check_sub_boards(OctagonBoard,Width,Height,Player,Graph,AlternativeCount,CurrentAlternative) :-
    LowBar is CurrentAlternative - 1,
    HighBar is LowBar + Height - AlternativeCount,
    % write('-- board_iter2 - Number of alternatives: '), write(AlternativeCount), write(' Lowbar: '),write(LowBar), write(' Highbar: '),write(HighBar),nl,
    
    get_valid_starters(OctagonBoard,Player,LowBar,Width,Starters),

    gen_row_ids(Width,HighBar,IDList),
    % print(Starters),nl,
    % print(IDList),nl,
    % print(Graph),nl,
    
    \+reachable_from_list(Graph,Starters,IDList),!,
    % write('not reachable'),nl,
    CurrentAlternative1 is CurrentAlternative - 1, 
    check_sub_boards(OctagonBoard,Width,Height,Player,Graph,AlternativeCount,CurrentAlternative1).

check_sub_boards(OctagonBoard,Width,Height,Player,Graph,AlternativeCount,CurrentAlternative) :-!.