generate_initial_game_state(Width, Height, P1Type, P2Type, [OctagonBoard, SquareBoard, Height, Width, P1Type, P2Type, 1, 1-0 |[]]) :-
    Width > 2,
    Height > 2,
    generate_octagon_board(Width, Height, OctagonBoard),
    generate_square_board(Width, Height, SquareBoard).

get_game_attributes(GameState, OctagonBoard, SquareBoard, Height, Width, P1Type, P2Type, CurrentPlayer, Cut) :-
    GameState = [OctagonBoard, SquareBoard, Height, Width, P1Type, P2Type, CurrentPlayer, Cut | []].

get_game_current_player(GameState, CurrentPlayer) :-
    GameState = [_, _, _, _, _, _, CurrentPlayer | _ ].

get_game_current_player_type(GameState, Type) :-
    get_game_current_player(GameState, Player),
    PlayerTypeIndex is 3 + Player,
    nth0(PlayerTypeIndex, GameState, Type).

get_game_board_size(GameState, Height, Width) :-
    GameState = [_, _, Height, Width | _].

get_game_octagon_board(GameState, OctagonBoard) :-
    GameState = [OctagonBoard | _].


generate_octagon_board(Width, Height, OctagonBoard) :-
    generate_octagon_board_row(Width, Row),
    generate_octagon_board_aux(Height, Row, OctagonBoard).

generate_octagon_board_aux(0, _, []).
generate_octagon_board_aux(Height, Row, [Row | OctagonBoard]) :-
    NextHeight is Height - 1,
    generate_octagon_board_aux(NextHeight, Row, OctagonBoard).

generate_octagon_board_row(0, []).
generate_octagon_board_row(Width, [0 | Row]) :-
    Width > 0,
    NextWidth is Width - 1,
    generate_octagon_board_row(NextWidth, Row).

generate_square_board(Width, Height, SquareBoard) :-
    generate_square_board_first_and_last_row(Width, 0, FirstLastRow),
    generate_square_board_middle_row(Width, 0, MiddleRow),
    generate_square_board_aux(Height, 0, FirstLastRow, MiddleRow, SquareBoard).

generate_square_board_aux(Height, 0, FirstLastRow, MiddleRow,  [FirstLastRow | SquareBoard]) :-
    generate_square_board_aux(Height, 1, FirstLastRow, MiddleRow, SquareBoard).

generate_square_board_aux(Height, Height, FirstLastRow, _, [FirstLastRow | []]).

generate_square_board_aux(Height, CurrentVerticalCoord, FirstLastRow, MiddleRow, [MiddleRow | SquareBoard]) :-
    CurrentVerticalCoord > 0,
    CurrentVerticalCoord < Height,
    NextVerticalCoord is CurrentVerticalCoord + 1,
    generate_square_board_aux(Height, NextVerticalCoord, FirstLastRow, MiddleRow, SquareBoard).

generate_square_board_first_and_last_row(Width, 0, [0 | Row]) :-
    generate_square_board_first_and_last_row(Width, 1, Row).

generate_square_board_first_and_last_row(Width, Width, [0 | []]).

generate_square_board_first_and_last_row(Width, CurrentHorizontalCoord, [1 | Row]) :-
    CurrentHorizontalCoord > 0,
    CurrentHorizontalCoord < Width,
    NextHorizontalCoord is CurrentHorizontalCoord + 1,
    generate_square_board_first_and_last_row(Width, NextHorizontalCoord, Row).

generate_square_board_middle_row(Width, 0, [2 | Row]) :-
    generate_square_board_middle_row(Width, 1, Row).

generate_square_board_middle_row(Width, Width, [2 | []]).

generate_square_board_middle_row(Width, CurrentHorizontalCoord, [0 | Row]) :-
    CurrentHorizontalCoord > 0,
    CurrentHorizontalCoord < Width,
    NextHorizontalCoord is CurrentHorizontalCoord + 1,
    generate_square_board_middle_row(Width, NextHorizontalCoord, Row).
     