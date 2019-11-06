get_diagonals_pos(X-Y, Res) :-
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

get_element_at(Board, X-Y, Element) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Element).

update_squares(Player, X-Y, OctagonBoard, SquareBoard, NewSquareBoard) :-
    get_diagonals_pos(X-Y, DiagonalsPos),
    get_squares_pos(Player, OctagonBoard, DiagonalsPos, SquaresPos),
    place_squares(Player, Res, OctagonBoard, SquareBoard, NewSquareBoard).

get_squares_pos(_, _, _, [], []).

get_squares_pos(Player, OctagonBoard, X-Y, [DiagonalX-DiagonalY | DiagonalsPos], [SquareX-SquareY | SquaresPos]) :-
    Player == get_element_at(OctagonBoard, DiagonalX-DiagonalY, Player),
    max(X, DiagonalX, SquareX),
    max(Y, DiagonalY, SquareY),
    get_squares_pos(Player, OctagonBoard, X-Y, DiagonalsPos, SquaresPos).

get_squares_pos(Player, OctagonBoard, X-Y, [DiagonalX-DiagonalY | DiagonalsPos], SquaresPos) :-
    Player \== get_element_at(OctagonBoard, DiagonalX-DiagonalY, Player),
    max(X, DiagonalX, SquareX),
    max(Y, DiagonalY, SquareY),
    get_squares_pos(Player, OctagonBoard, X-Y, DiagonalsPos, SquaresPos).


max(V1, V2, M) :- V1 > V2, M = V1.
max(V1, V2, M) :- V2 > V1, M = V2.