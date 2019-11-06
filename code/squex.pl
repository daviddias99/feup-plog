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
move(Player, X-Y, [OctagonBoard, SquareBoard | []], [NewOctagonBoard, SquareBoard | []]) :-
    valid_moves(OctagonBoard, Moves),
    member(X-Y, Moves),
    board_insert_element_at(OctagonBoard, X, Y, Player, NewOctagonBoard).


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

