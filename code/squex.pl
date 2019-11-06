:- [display], [input].

start(Player) :- init_board(Board), display_game(Board, Player).

init_board([[
             [0, 0, 0, 0, 0, 0, 0, 0],
             [1, 2, 2, 1, 1, 1, 1, 1],
             [2, 1, 2, 2, 1, 1, 2, 1],
             [1, 2, 2, 1, 2, 1, 2, 1],
             [2, 2, 1, 2, 1, 2, 2, 1],
             [1, 1, 0, 1, 2, 1, 0, 1],
             [2, 1, 2, 0, 1, 2, 1, 0],
             [1, 1, 0, 0, 1, 1, 2, 2]
            ],
            [
             [0, 1, 1, 1, 1, 1, 1, 1, 0],
             [2, 0, 0, 0, 1, 0, 0, 0, 2],
             [2, 0, 0, 0, 1, 1, 1, 1, 2],
             [2, 0, 0, 0, 0, 1, 0, 0, 2],
             [2, 0, 0, 1, 1, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 1, 0, 0, 2],
             [2, 0, 0, 0, 0, 1, 1, 1, 2],
             [2, 0, 0, 0, 0, 1, 0, 0, 2],
             [0, 1, 1, 1, 1, 1, 1, 1, 0]
            ]]
).

test_board([[
    [0, 0, 0],
    [0, 0, 0],
    [0, 0, 0]
] , [
    [0, 1, 1, 0],
    [2, 0, 0, 2],
    [2, 0, 0, 2],
    [0, 1, 1, 0]
]]).

% valid_moves(+OctagonBoard, +Player, -ListOfMoves)​
valid_moves_row(_NumRow, [], _Moves).
valid_moves_row(NumRow, Row, Moves) :-
    findall(NumCol-NumRow, nth0(NumCol, Row, 0), Moves).

valid_moves(OctagonBoard, Moves) :-
    valid_moves_aux(OctagonBoard, 0, Moves, []).

valid_moves_aux([], _NumRow, Moves, Moves).

valid_moves_aux([Row | OctagonBoard], NumRow, Moves, MovesAcc) :-
    valid_moves_row(NumRow, Row, MovesRow),
    append(MovesAcc, MovesRow, NewMovesAcc),
    NewNumRow is NumRow + 1,
    valid_moves_aux(OctagonBoard, NewNumRow, Moves, NewMovesAcc).

% move(+Move, +Board, -NewBoard).
move(Player, X-Y, [OctagonBoard, SquareBoard | []], [NewOctagonBoard, NewSquareBoard | []]) :-
    valid_moves(OctagonBoard, Moves),
    member(X-Y, Moves),
    board_insert_element_at(OctagonBoard, X, Y, Player, NewOctagonBoard),
    update_squares(Player, X-Y, OctagonBoard, SquareBoard, NewSquareBoard).


board_insert_element_at(Board, X, Y, Element, NewBoard) :-
    nth0(Y, Board, Row),
    insert_element_at(Row, X, Element, NewRow),
    insert_element_at(Board, Y, NewRow, NewBoard).

insert_element_at(Row, X, Element, NewRow) :-
    insert_element_at_aux(Row, X, Element, NewRow, 0).

insert_element_at_aux([_HRow | TRow], X, Element, [Element | NewRow], XCount) :-
    X == XCount,
    NextXCount is XCount + 1,
    insert_element_at_aux(TRow, X, Element, NewRow, NextXCount).

insert_element_at_aux([HRow | TRow], X, Element, [HRow | NewRow], XCount) :-
    X \== XCount,
    NextXCount is XCount + 1,
    insert_element_at_aux(TRow, X, Element, NewRow, NextXCount).

insert_element_at_aux([], _, _, [], _).

get_diagonals_pos(0-0, Res) :-
    Res = [
        1-1
    ].

get_diagonals_pos(0-Y, Res) :-
    XRight is 1,
    YUp is Y - 1,
    YBottom is Y + 1,
    Res = [
        XRight-YUp,
        XRight-YBottom
    ].

get_diagonals_pos(X-0, Res) :-
    XLeft is X - 1,
    XRight is X + 1,
    YBottom is 1,
    Res = [
        XLeft-YBottom,
        XRight-YBottom
    ].

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
    get_squares_pos(Player, OctagonBoard, X-Y, DiagonalsPos, SquaresPos),
    place_squares(Player, SquareBoard, SquaresPos, NewSquareBoard).

get_squares_pos(_, _, _, [], []).

get_squares_pos(Player, OctagonBoard, X-Y, [DiagonalX-DiagonalY | DiagonalsPos], [SquareX-SquareY | SquaresPos]) :-
    get_element_at(OctagonBoard, DiagonalX-DiagonalY, Element),
    Player == Element,
    max(X, DiagonalX, SquareX),
    max(Y, DiagonalY, SquareY),
    get_squares_pos(Player, OctagonBoard, X-Y, DiagonalsPos, SquaresPos).

get_squares_pos(Player, OctagonBoard, X-Y, [DiagonalX-DiagonalY | DiagonalsPos], SquaresPos) :-
    get_element_at(OctagonBoard, DiagonalX-DiagonalY, Element),
    Player \== Element, 
    get_squares_pos(Player, OctagonBoard, X-Y, DiagonalsPos, SquaresPos).

place_squares(_, SquareBoard, [], SquareBoard).
place_squares(Player, SquareBoard, [X-Y | SquaresPos], FinalSquareBoard) :-
    board_insert_element_at(SquareBoard, X, Y, Player, NewSquareBoard),
    place_squares(Player, NewSquareBoard, SquaresPos, FinalSquareBoard).
    
max(V1, V2, M) :- V1 > V2, M = V1.
max(V1, V2, M) :- V2 > V1, M = V2.