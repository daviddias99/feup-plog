generate_board(HorizontalSize, VerticalSize, [OctagonBoard, SquareBoard |[]]) :-
    HorizontalSize > 2,
    VerticalSize > 2,
    generate_octagon_board(HorizontalSize, VerticalSize, OctagonBoard),
    generate_square_board(HorizontalSize, VerticalSize, SquareBoard).


generate_octagon_board(HorizontalSize, VerticalSize, OctagonBoard) :-
    generate_octagon_board_row(HorizontalSize, Row),
    generate_octagon_board_aux(VerticalSize, Row, OctagonBoard).

generate_octagon_board_aux(0, _, []).
generate_octagon_board_aux(VerticalSize, Row, [Row | OctagonBoard]) :-
    NextVerticalSize is VerticalSize - 1,
    generate_octagon_board_aux(NextVerticalSize, Row, OctagonBoard).

generate_octagon_board_row(0, []).
generate_octagon_board_row(HorizontalSize, [0 | Row]) :-
    HorizontalSize > 0,
    NextHorizontalSize is HorizontalSize - 1,
    generate_octagon_board_row(NextHorizontalSize, Row).

generate_square_board(HorizontalSize, VerticalSize, SquareBoard) :-
    generate_square_board_first_and_last_row(HorizontalSize, 0, FirstLastRow),
    generate_square_board_middle_row(HorizontalSize, 0, MiddleRow),
    generate_square_board_aux(VerticalSize, 0, FirstLastRow, MiddleRow, SquareBoard).

generate_square_board_aux(VerticalSize, 0, FirstLastRow, MiddleRow,  [FirstLastRow | SquareBoard]) :-
    generate_square_board_aux(VerticalSize, 1, FirstLastRow, MiddleRow, SquareBoard).

generate_square_board_aux(VerticalSize, VerticalSize, FirstLastRow, _, [FirstLastRow | []]).

generate_square_board_aux(VerticalSize, CurrentVerticalCoord, FirstLastRow, MiddleRow, [MiddleRow | SquareBoard]) :-
    CurrentVerticalCoord > 0,
    CurrentVerticalCoord < VerticalSize,
    NextVerticalCoord is CurrentVerticalCoord + 1,
    generate_square_board_aux(VerticalSize, NextVerticalCoord, FirstLastRow, MiddleRow, SquareBoard).

generate_square_board_first_and_last_row(HorizontalSize, 0, [0 | Row]) :-
    generate_square_board_first_and_last_row(HorizontalSize, 1, Row).

generate_square_board_first_and_last_row(HorizontalSize, HorizontalSize, [0 | []]).

generate_square_board_first_and_last_row(HorizontalSize, CurrentHorizontalCoord, [1 | Row]) :-
    CurrentHorizontalCoord > 0,
    CurrentHorizontalCoord < HorizontalSize,
    NextHorizontalCoord is CurrentHorizontalCoord + 1,
    generate_square_board_first_and_last_row(HorizontalSize, NextHorizontalCoord, Row).

generate_square_board_middle_row(HorizontalSize, 0, [2 | Row]) :-
    generate_square_board_middle_row(HorizontalSize, 1, Row).

generate_square_board_middle_row(HorizontalSize, HorizontalSize, [2 | []]).

generate_square_board_middle_row(HorizontalSize, CurrentHorizontalCoord, [0 | Row]) :-
    CurrentHorizontalCoord > 0,
    CurrentHorizontalCoord < HorizontalSize,
    NextHorizontalCoord is CurrentHorizontalCoord + 1,
    generate_square_board_middle_row(HorizontalSize, NextHorizontalCoord, Row).
     