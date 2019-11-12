:- use_module(library(clpfd)).
:- [graph].

%
% gameover(+Board,-Player) - checks if the game is over, if so returns the player that won
% Oct, Sq, Height, Width, P1T, P2T, Player, NumJogadas

% first, gameover checks if there is a path that unites Player's board edge-rows


gameover([OctagonBoard,SquareBoard,_,_,_,_,NextPlayer,_-CutFlag|[]],Player) :- 


    %  If CutFlag is 1, than we the last play was the first play of a double play
    %  as such, the NextPlayer is the same one that just played, so the player that
    %  who's win must be checked is NextPlayer.
    CutFlag =:= 1,

    check_for_win([OctagonBoard,SquareBoard],NextPlayer).

gameover([OctagonBoard,SquareBoard,_,_,_,_,NextPlayer,_-CutFlag|[]],Player) :- 

    %  If CutFlag is 0 than the next player is the opponent of the player that played the previous play,
    %  as such the player who's win must be checked is the opponent of NextPlayer.
    CutFlag =:= 0,
    get_other_player(NextPlayer,Player);
    check_for_win([OctagonBoard,SquareBoard],Player).

get_other_player(1,2).
get_other_player(2,1).

check_for_win(Board,Player) :-

    % First condition for winning is if there is a path unitting both colored edges of a player
    test_for_path(Board,Player),
    get_other_player(Player,Opponent),


    % Second condition for winning is that there is no possible way for the oponnent to destroy said path
    % Find all moves (PMove) possible by the Opponent such that there is a cut and that causes Player to not have any
    % available paths. If there are no such moves, Player won.
    findall(PMove, (move(Opponent,PMove,Board,NextBoard,NumCuts),NumCuts =\= 0,\+test_for_path(NextBoard,Player)),Result),
    Result = [].

% check if there is a path uniting Player's board edge-rows (search starts at the upper row)
% get the starters (octagon pieces of Player that are touching it's upper row)
% build the connecting graph
test_for_path([OctagonBoard,SquareBoard | _],Player):-
    Player =:= 1,
    get_valid_starters(OctagonBoard,Player,Starters),
    build_graph(OctagonBoard,SquareBoard,Player,Graph),
    reachable_from_starters(Graph,Starters).

test_for_path([OctagonBoard,SquareBoard | _],Player):-
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