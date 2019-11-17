:- ensure_loaded('game_model.pl').

valid_moves(GameState, Moves) :-
    get_game_octagon_board(GameState, OctagonBoard),
    findall(Move, validate_move(OctagonBoard, Move), Moves).

validate_move(OctagonBoard, Move) :-
    board_get_element_at(OctagonBoard, Move, 0).

% move(+Move, +Board, -NewBoard).
move(Move,GameState,NewGameState) :-
    get_game_attributes(GameState, OctagonBoard, SquareBoard, Height, Width, P1Type, P2Type, Player, CutInfo), 
    validate_move(OctagonBoard, Move),
    board_insert_element_at(OctagonBoard, Move, Player, NewOctagonBoard),
    update_squares(Player, Move, OctagonBoard, SquareBoard, NewSquareBoard, Height, Width, NumCuts),
    update_next_player(Player, CutInfo, NewPlayer, NewCutInfo, NumCuts),
    get_game_attributes(NewGameState, NewOctagonBoard, NewSquareBoard, Height, Width, P1Type, P2Type, NewPlayer, NewCutInfo).

update_next_player(1, _, 2, 2-1, NumCuts) :- NumCuts > 0.
update_next_player(2, _, 1, 2-1, NumCuts) :- NumCuts > 0.
update_next_player(1, 2-1, 1, 1-1, NumCuts) :- NumCuts == 0. 
update_next_player(2, 2-1, 2, 1-1, NumCuts) :- NumCuts == 0.
update_next_player(2, 1-1, 1, 1-0, NumCuts) :- NumCuts == 0.
update_next_player(1, 1-1, 2, 1-0, NumCuts) :- NumCuts == 0.
update_next_player(2, 1-0, 1, 1-0, NumCuts) :- NumCuts == 0.
update_next_player(1, 1-0, 2, 1-0, NumCuts) :- NumCuts == 0.

get_diagonals_pos(0-0, _, _, [1-1]).
get_diagonals_pos(X-0, _, Width, [CoordX-1]) :-
    X =:= Width - 1,
    CoordX is X - 1.

get_diagonals_pos(0-Y, Height, _, [1-CoordY]) :-
    Y =:= Height - 1,
    CoordY is Y - 1.


get_diagonals_pos(X-Y, Height, Width, [CoordX-CoordY]) :-
    Y =:= Height - 1,
    X =:= Width - 1,
    CoordX is X - 1,
    CoordY is Y - 1.

get_diagonals_pos(X-Y, _, Width, Res) :-
    X =:= Width - 1,
    XLeft is X - 1,
    YUp is Y - 1,
    YBottom is Y + 1,
    Res = [
        XLeft-YUp,
        XLeft-YBottom
    ].

get_diagonals_pos(X-Y, Height, _, Res) :-
    Y =:= Height - 1,
    YTop is Y - 1,
    XLeft is X - 1,
    XRight is X + 1,
    Res = [
        XLeft-YTop,
        XRight-YTop
    ].


get_diagonals_pos(0-Y, _, _, Res) :-
    XRight is 1,
    YUp is Y - 1,
    YBottom is Y + 1,
    Res = [
        XRight-YUp,
        XRight-YBottom
    ].

get_diagonals_pos(X-0, _, _, Res) :-
    XLeft is X - 1,
    XRight is X + 1,
    YBottom is 1,
    Res = [
        XLeft-YBottom,
        XRight-YBottom
    ].

get_diagonals_pos(X-Y, _, _, Res) :-
    XLeft is X - 1,
    XRight is X + 1,
    YUp is Y - 1,
    YBottom is Y + 1,
    Res = [
        XLeft-YUp,
        XRight-YUp,
        XLeft-YBottom,
        XRight-YBottom
    ].

update_squares(Player, Pos, OctagonBoard, SquareBoard, NewSquareBoard, Height, Width, NumCuts) :-
    get_diagonals_pos(Pos, Height, Width, DiagonalsPos),
    get_squares_pos(Player, OctagonBoard, Pos, DiagonalsPos, SquaresPos),
    check_cut(Player, SquareBoard, SquaresPos, NumCuts),
    place_squares(Player, SquareBoard, SquaresPos, NewSquareBoard).


check_cut(Player, SquareBoard, SquaresPos, NumCuts) :- 
    findall(Pos, (member(Pos, SquaresPos), board_get_element_at(SquareBoard, Pos, Element), Element \== Player, Element \== 0), Cuts),
    length(Cuts, NumCuts).




get_squares_pos(_, _, _, [], []).

get_squares_pos(Player, OctagonBoard, X-Y, [DiagonalX-DiagonalY | DiagonalsPos], [SquareX-SquareY | SquaresPos]) :-
    board_get_element_at(OctagonBoard, DiagonalX-DiagonalY, Element),
    Player == Element,
    max(X, DiagonalX, SquareX),
    max(Y, DiagonalY, SquareY),
    get_squares_pos(Player, OctagonBoard, X-Y, DiagonalsPos, SquaresPos).

get_squares_pos(Player, OctagonBoard, X-Y, [DiagonalX-DiagonalY | DiagonalsPos], SquaresPos) :-
    board_get_element_at(OctagonBoard, DiagonalX-DiagonalY, Element),
    Player \== Element, 
    get_squares_pos(Player, OctagonBoard, X-Y, DiagonalsPos, SquaresPos).

place_squares(_, SquareBoard, [], SquareBoard).
place_squares(Player, SquareBoard, [Pos | SquaresPos], FinalSquareBoard) :-
    board_insert_element_at(SquareBoard, Pos, Player, NewSquareBoard),
    place_squares(Player, NewSquareBoard, SquaresPos, FinalSquareBoard).
    
max(V1, V2, M) :- V1 > V2, M = V1.
max(V1, V2, M) :- V2 > V1, M = V2.