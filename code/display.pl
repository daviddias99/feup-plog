% display board

display_board([OctagonBoard, SquareBoard |[]]) :-
    display_horizontal_coordinates(a, 8), nl, 
    display_board(OctagonBoard, SquareBoard, 0).

display_board([], [], _).
display_board([], [SquareRow | SquareBoard], Y) :- 
    display_square_row_borders(SquareRow, Y), 
    YNext is Y + 1,
    display_board([], SquareBoard, YNext).
display_board([OctagonRow | OctagonBoard], [SquareRow | SquareBoard], Y) :-
    display_square_row_borders(SquareRow, Y), nl,
    ansi_format(bold, Y, [world]), write(' '),  
    display_octagon_hor_separator, 
    display_octagon_row(OctagonRow), nl,
    YNext is Y + 1,
    display_board(OctagonBoard, SquareBoard, YNext).


% display pieces

display_square_piece(1) :- ansi_format(fg(blue), '\u25C6', [world]).
display_square_piece(2) :- ansi_format(fg(red), '\u25C6', [world]).
display_square_piece(_) :- ansi_format([], ' ', [world]).

display_octagon_piece(1) :- ansi_format(fg(blue), '\u2BC3', [world]).
display_octagon_piece(2) :- ansi_format(fg(red), '\u2BC3', [world]).
display_octagon_piece(_) :- ansi_format([], ' ', [world]).


% display rows

display_octagon_row([]).
display_octagon_row([H | T]) :- display_octagon_piece(H), display_octagon_hor_separator, display_octagon_row(T). 

display_square_row_borders(SquareRow, Y) :-
    write('    '), display_lower_octagon_cell(Y), nl,
    write('     '), display_square_row(SquareRow), nl,
    write('    '), display_upper_octagon_cell(Y).

display_square_row([]).
display_square_row([H | []]) :- display_square_piece(H).
display_square_row([H | T]) :- display_square_piece(H), display_octagon_ver_separator, display_square_row(T).


% auxiliary functions to display borders of the cells

display_upper_octagon_cell(0) :- ansi_format(bold, '  \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 ', [world]).
display_upper_octagon_cell(8) :- ansi_format(bold, '        \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571 ', [world]).
display_upper_octagon_cell(_) :- ansi_format(bold, '\u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571     \u2572 \u2571', [world]).

display_lower_octagon_cell(0) :- ansi_format(bold, '        \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572', [world]).
display_lower_octagon_cell(8) :- ansi_format(bold, '  \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 ', [world]).
display_lower_octagon_cell(_) :- ansi_format(bold, '\u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572     \u2571 \u2572', [world]).

display_octagon_ver_separator :- write('  \u2501\u2501\u2501  ').
display_octagon_hor_separator :- write('   \u2503   ').


% display coordinates

display_horizontal_coordinates(LastChar, 1) :- write('   '), ansi_format(bold, LastChar, [world]), nl.
display_horizontal_coordinates(a, Num) :-
    Num > 1, 
    write('         '), 
    ansi_format(bold, a, [world]), 
    write('    '),  
    char_code(a, Code),
    NewCode is Code + 1,
    NewNum is Num - 1,
    char_code(NewChar, NewCode),
    display_horizontal_coordinates(NewChar, NewNum).
display_horizontal_coordinates(Char, Num) :- 
    Num > 1,
    write('   '), 
    ansi_format(bold, Char, [world]), 
    write('    '), 
    char_code(Char, Code),
    NewCode is Code + 1,
    NewNum is Num - 1,
    char_code(NewChar, NewCode),
    display_horizontal_coordinates(NewChar, NewNum).


