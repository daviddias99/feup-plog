:- ensure_loaded('game_logic.pl').
:- ensure_loaded('game_model.pl').

random_move(GameState, Move) :-
    valid_moves(GameState, ListOfMoves),
    random_member(Move, ListOfMoves).
