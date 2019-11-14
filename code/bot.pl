:- ensure_loaded('game_logic.pl').
:- ensure_loaded('game_model.pl').

random_move(GameState, Move) :-
    valid_moves(GameState, ListOfMoves),
    random_member(Move, ListOfMoves).

greedy_move(GameState, _) :-
    valid_moves(GameState,ListOfMoves),
    setof(Value-Move, (member(Move, ListOfMoves),move(Move, GameState, NewGameState), value(NewGameState, Value)), Result).

value(GameState, Value) :-
    GameState = [OctagonBoard,SquareBoard, Height, Width, _, _, Player |_],
    build_graph([OctagonBoard, SquareBoard, Height, Width], Player, Graph),
    
