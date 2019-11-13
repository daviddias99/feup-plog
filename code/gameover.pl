:- use_module(library(clpfd)).
:- [graph].
:- ensure_loaded('game_model.pl').

%
% gameover(+Board,-Player) - checks if the game is over, if so returns the player that won
% Oct, Sq, Height, Width, P1T, P2T, Player, NumJogadas

% first, gameover checks if there is a path that unites Player's board edge-rows

%------------------------------------------ 2,1,0
%------------------------------------------ 1,2,1
%------------------------------------------ 1,1,1
%------------------------------------------ 2,1,0
gameover(GameState,Player) :- 
    get_game_previous_player(GameState,Player),
    check_for_win(GameState,Player).

check_for_win(GameState,Player) :-

    % First condition for winning is if there is a path unitting both colored edges of a player
    test_for_path(GameState,Player),
    get_other_player(Player,Opponent),

    % Second condition for winning is that there is no possible way for the oponnent to destroy said path
    % Find all moves (PMove) possible by the Opponent such that there is a cut and that causes Player to not have any
    % available paths. If there are no such moves, Player won.
    findall(PMove, (move(Opponent,PMove,GameState,NextGameState,NumCuts),NumCuts =\= 0,\+test_for_path(NextGameState,Player)),Result),
    Result = [].

% check if there is a path uniting Player's board edge-rows (search starts at the upper row)
% get the starters (octagon pieces of Player that are touching it's upper row)
% build the connecting graph
test_for_path([OctagonBoard,SquareBoard,Height,Width | _],Player):-
    Player =:= 1,
    get_valid_starters(OctagonBoard,Player,Starters),
    build_graph([OctagonBoard,SquareBoard,Height,Width],Player,Graph),
    generate_last_tiles_id_list(Width,Height,IDList),
    reachable_from_starters(Graph,Starters,IDList).

test_for_path([OctagonBoard,SquareBoard,Height,Width | _],Player):-
    Player =:= 2,
    transpose(OctagonBoard,TransposedOctagonBoard), 
    transpose(SquareBoard,TransposedSquareBoard),
    get_valid_starters(TransposedOctagonBoard,Player,Starters),
    build_graph([TransposedOctagonBoard,TransposedSquareBoard,Height,Width],Player,Graph),
    generate_last_tiles_id_list(Width,Height,IDList),
    reachable_from_starters(Graph,Starters,IDList).

% check if the opposing edge is reachable from any of the starting octagons
reachable_from_starters(Graph,[H|_],IDList) :-
    reachable(H,Graph,Result),
    contains_end(Result,IDList).

reachable_from_starters(Graph,[H|T],IDList) :-
    reachable(H,Graph,Result),
    \+contains_end(Result,IDList),
    reachable_from_starters(Graph,T).

% check if a list contains any of the octagons from the end row
contains_end([],_,_) :- fail.
contains_end([H|T],IDList) :- \+intersection([H|T],IDList,[]).


% generate list that contains the ids of octagons of the last row of the octagon board
generate_last_tiles_id_list(Width,Height,IDList) :- generate_last_tiles_id_list_iter(Width,Height,IDList,[]).

generate_last_tiles_id_list_iter(_,_,Result,Result).

generate_last_tiles_id_list_iter(Width,Height,Result,Acc) :-

    length(Acc,Size),
    Element is Width * (Height - 1) + Size,
    append(Acc,[Element],Acc1),
    generate_last_tiles_id_list_iter(Width,Height,Result,Acc1).


% get in Starters a list with the indexes of the valid piece starting points
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