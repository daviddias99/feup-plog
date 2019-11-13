:- use_module(library(clpfd)).
:- [graph].
:- ensure_loaded('game_model.pl').


%   In this file:
% 
% -  gameover(+Board,-Player)
% -  test_for_path(+Gamestate,+Player)
% -  orient_board(+OctagonBoard,+SquareBoard,Player,-OrientedOctagonBoard,-OrientedSquareBoard)
% -  reachable_from_starters(+Graph,+Starters,+Destinations)
% -  contains_any(+X,+Y)
% -  gen_last_row_ids(+BoardWidth,+BoardHeight,-IDList)
% -  get_valid_starters(+OctagonBoard,+Player,-Starters)


/**
*   gameover(+Board,-Player) 
*
*   Checks if the game is over, if so returns the player that won.
*/

gameover(GameState,Player) :- 
    get_game_previous_player(GameState,Player),
    check_for_win(GameState,Player).

/**
*   check_for_win(+Gamestate, +Player)   
*   
*   Given a game state, check_for_win checks if Player won the game
*/
check_for_win(GameState,Player) :-

    % First condition for winning is if there is a path unitting both colored edges of a player
    test_for_path(GameState,Player),
    get_other_player(Player,Opponent),

    % Second condition for winning is that there is no possible way for the oponnent to destroy said path
    % Find all moves (PMove) possible by the Opponent such that there is a cut and that causes Player to not have any
    % available paths. If there are no such moves, Player won.
    findall(PMove, (move(Opponent,PMove,GameState,NextGameState,NumCuts),NumCuts =\= 0,\+test_for_path(NextGameState,Player)),Result),
    Result = [].


/**
*   test_for_path(+Gamestate,+Player)
*
*   Test if, given the Gamestate, that Player as a path (octagons and/or squares) that connects his two board edges
*/
test_for_path([OctagonBoard,SquareBoard,Height,Width | _],Player):-
    Player =:= 1,
    orient_board(OctagonBoard,SquareBoard,OrientedOctagonBoard,OrientedSquareBoard),

    % Get the octagons where the path may start(there are connected with the Player's top board edge). This function is meant to optimize the 
    % process as there is no need to build the graph(which is an operation that is computationally expensive) if there is no starting pice to begin with.

    get_valid_starters(OrientedOctagonBoard,Player,Starters),

    % Build the player-path graph
    build_graph([OrientedOctagonBoard,OrientedSquareBoard,Height,Width],Player,Graph),

    % Check if the opposing board edge is connected to the starting one via a path
    gen_last_row_ids(Width,Height,LastRowIDs),
    reachable_from_starters(Graph,Starters,LastRowIDs).

/**
*   orient_board(+OctagonBoard,+SquareBoard,Player,-OrientedOctagonBoard,-OrientedSquareBoard)
*
*   If Player is 2, than the oriented boards are the transposed input boards.To avoid writting redundant code there was a choice to make the graph functions
*   process the board vertically, as such, for it to be able to process player 2 it must used the transposed version of the board.
*/
orient_board(OctagonBoard,SquareBoard,1, OctagonBoard, SquareBoard).

orient_board(OctagonBoard,SquareBoard,2, NewOctagonBoard, NewSquareBoard) :-
    transpose(OctagonBoard,NewOctagonBoard), 
    transpose(SquareBoard,NewSquareBoard).

/**
*   reachable_from_starters(+Graph,+Starters,+Destinations)
*
*   This predicate succeeds if, following Graph, there is a path that connects an octagon from Starters with an Octagon from Destinations
*/
reachable_from_starters(Graph,[H|_],Destinations) :-
    reachable(H,Graph,Result),
    contains_any(Result,Destinations).

reachable_from_starters(Graph,[H|T],Destinations) :-
    reachable(H,Graph,Result),
    \+contains_end(Result,Destinations),
    reachable_from_starters(Graph,T).

/**
*   contains_any(+X,+Y)
*
*   Checks if any element from the list X is present in list Y. If X is an empty list the predicate will fail.
*/
contains_any([],_,_) :- fail.
contains_any([H|T],L) :- \+intersection([H|T],L,[]).


/**
*   gen_last_row_ids(+BoardWidth,+BoardHeight,-IDList)
*
*   Fills IDList with the ids of the cells of the last row of the Board with size BoardWidth/BoardHeight
*/
gen_last_row_ids(Width,Height,IDList) :-gen_last_row_ids_iter(Width,Height,IDList,[]).

gen_last_row_ids_iter(_,_,Result,Result).

gen_last_row_ids_iter(Width,Height,Result,Acc) :-

    length(Acc,Size),
    Element is Width * (Height - 1) + Size,
    append(Acc,[Element],Acc1),
    gen_last_row_ids_iter(Width,Height,Result,Acc1).


/**
*   get_valid_starters(+OctagonBoard,+Player,-Starters)
*
*   Fill Starters with the ID's of the pieces of the first row of OctagonBoard that belong to Player
*/
get_valid_starters([H|_],Player,Starters) :- member(Player,H), fetch_starters(H,Player,Starters).

fetch_starters(Row, Player, Result) :- fetch_starters_iter(Row,Player,Result,[],0).

fetch_starters_iter([],_,Result,Result,_).
fetch_starters_iter([H|T],Player,Result,Acc,N) :-  
        H =:= Player, 
        append(Acc,[N],Acc1),
        N1 is N + 1, 
        fetch_starters_iter(T,Player,Result,Acc1,N1).

fetch_starters_iter([H|T],Player,Result,Acc,N) :-  
        H =\= Player, 
        N1 is N + 1, 
        fetch_starters_iter(T,Player,Result,Acc,N1).