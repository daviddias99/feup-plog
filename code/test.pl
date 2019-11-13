remove_cuttable_squares(OctagonBoard, [Row | SquareBoard], Player, [Row | NewSquareBoard]) :-
    remove_cuttable_squares_aux(OctagonBoard, SquareBoard, Player, 1, NewSquareBoard).

remove_cuttable_squares_aux(_OctagonBoard, [Row | []], _Y, _Player, [Row]).

remove_cuttable_squares_aux(OctagonBoard, [Row | SquareBoard], Y, Player, [NewRow | NewSquareBoard]) :-
    remove_cuttable_squares_row(OctagonBoard, Row, Y, Player, NewRow),
    YNext is Y + 1, 
    remove_cuttable_squares_aux(OctagonBoard, SquareBoard, YNext, Player, NewSquareBoard).

remove_cuttable_squares_row(OctagonBoard, [Element | Row], Y, Player, [Element | NewRow]) :-
    remove_cuttable_squares_row_aux(OctagonBoard, Row, 1, Y, Player, NewRow).

remove_cuttable_squares_row_aux(_OctagonBoard, [Element | []], _X, _Y, _Player, [Element]).

% Caso element seja cuttable
remove_cuttable_squares_row_aux(OctagonBoard, [Player | Row], X, Y, Player, [0 | NewRow]) :-
    is_cuttable(OctagonBoard, Player, X, Y),
    write(X), write('-'), write(Y), write(' is cuttable'), nl,
    XNext is X + 1,
    write(X), write('-'), write(Y), nl,
    remove_cuttable_squares_row_aux(OctagonBoard, Row, XNext, Y, Player, NewRow).

% Caso element n seja cuttable
remove_cuttable_squares_row_aux(OctagonBoard, [Player | Row], X, Y, Player, [Player | NewRow]) :-
    XNext is X + 1,
    write(X), write('-'), write(Y), nl,
    remove_cuttable_squares_row_aux(OctagonBoard, Row, XNext, Y, Player, NewRow).

% Caso element n seja do jogador a testar
remove_cuttable_squares_row_aux(OctagonBoard, [Element | Row], X, Y, Player, [Element | NewRow]) :-
    XNext is X + 1,
    write(X), write('-'), write(Y), nl,
    remove_cuttable_squares_row_aux(OctagonBoard, Row, XNext, Y, Player, NewRow).

%%%%%

is_cuttable(OctagonBoard, _Player, X, Y) :-
    board_get_element_at(OctagonBoard, X-Y, 0),
    OtherX is X - 1,
    OtherY is Y - 1,
    board_get_element_at(OctagonBoard, OtherX-OtherY, 0).

is_cuttable(OctagonBoard, _Player, X, Y) :-
    OtherX is X - 1,
    OtherY is Y - 1,
    board_get_element_at(OctagonBoard, X-OtherY, 0),
    board_get_element_at(OctagonBoard, OtherX-Y, 0).

is_cuttable(OctagonBoard, Player, X, Y) :-
    get_other_player(Player, OtherPlayer),
    board_get_element_at(OctagonBoard, X-Y, OtherPlayer),
    OtherX is X - 1,
    OtherY is Y - 1,
    board_get_element_at(OctagonBoard, OtherX-OtherY, 0).

is_cuttable(OctagonBoard, Player, X, Y) :-
    get_other_player(Player, OtherPlayer),
    OtherX is X - 1,
    OtherY is Y - 1,
    board_get_element_at(OctagonBoard, X-OtherY, OtherPlayer),
    board_get_element_at(OctagonBoard, OtherX-Y, 0).

is_cuttable(OctagonBoard, Player, X, Y) :-
    get_other_player(Player, OtherPlayer),
    board_get_element_at(OctagonBoard, X-Y, 0),
    OtherX is X - 1,
    OtherY is Y - 1,
    board_get_element_at(OctagonBoard, OtherX-OtherY, OtherPlayer).

is_cuttable(OctagonBoard, Player, X, Y) :-
    get_other_player(Player, OtherPlayer),
    OtherX is X - 1,
    OtherY is Y - 1,
    board_get_element_at(OctagonBoard, X-OtherY, 0),
    board_get_element_at(OctagonBoard, OtherX-Y, OtherPlayer).
