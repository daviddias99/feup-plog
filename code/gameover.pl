:- use_module(library(clpfd)).
:- [graph].

%
% gameover(+Board,+Player) - checks if Player as won.
%

% first, gameover checks if there is a path that unites Player's board edge-rows
gameover(Board,Player) :- 
    test_for_path(Board,Player),
    getOtherPlayer(Player,OtherPlayer),
    findall(PMove, (move(OtherPlayer,PMove,Board,NextBoard),\+test_for_path(NextBoard,Player) ),Result),
    Result = [].

getOtherPlayer(1,2).
getOtherPlayer(2,1).


% check if there is a path uniting Player's board edge-rows (search starts at the upper row)
% get the starters (octagon pieces of Player that are touching it's upper row)
% build the connecting graph
test_for_path([OctagonBoard,SquareBoard],Player):-
    Player =:= 1,
    get_valid_starters(OctagonBoard,Player,Starters),
    build_graph(OctagonBoard,SquareBoard,Player,Graph),
    reachable_from_starters(Graph,Starters).

test_for_path([OctagonBoard,SquareBoard],Player):-
    Player =:= 2,
    transpose(OctagonBoard,TransposedOctagonBoard), 
    transpose(SquareBoard,TransposedSquareBoard),
    get_valid_starters(TransposedOctagonBoard,Player,Starters),
    build_graph(TransposedOctagonBoard,TransposedSquareBoard,Player,Graph),
    reachable_from_starters(Graph,Starters).

% check if the opposing edge is reachable from any of the starting octagons
reachable_from_starters(Graph,[H|_]) :-
    reachable(H,Graph,Result),
    contains_end(Result).


reachable_from_starters(Graph,[H|T]) :-
    reachable(H,Graph,Result),
    \+contains_end(Result),
    reachable_from_starters(Graph,T).

% check if a list contains any of the octagons from the end row
contains_end([]) :- fail.
contains_end([H|T]) :- \+intersection([H|T],[56,57,58,59,60,61,62,63],[]).

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