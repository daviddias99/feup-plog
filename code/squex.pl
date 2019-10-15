start() :- init_board(Board), display_board(Board).


init_board([[
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0]
            ],
            [
             [' ', 1, 1, 1, 1, 1, 1, 1, ' '],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [' ', 1, 1, 1, 1, 1, 1, 1, ' ']
            ]]).

display_board([OctagonBoard, SquareBoard |[]]) :-
    display_horizontal_coordinates(a, 8), nl, 
    display_board(OctagonBoard, SquareBoard, 0).

display_horizontal_coordinates(X, 1) :- write('   '), write(X), nl.

display_horizontal_coordinates(a, Num) :-
    Num > 1, 
    write('         '), 
    write(a), 
    write('    '),  
    char_code(a, Code),
    NewCode is Code + 1,
    NewNum is Num - 1,
    char_code(NewChar, NewCode),
    display_horizontal_coordinates(NewChar, NewNum).

display_horizontal_coordinates(Char, Num) :- 
    Num > 1,
    write('   '), 
    write(Char), 
    write('    '), 
    char_code(Char, Code),
    NewCode is Code + 1,
    NewNum is Num - 1,
    char_code(NewChar, NewCode),
    display_horizontal_coordinates(NewChar, NewNum).


display_board([], [], _).

display_board([], [SquareLine | SquareBoard], Y) :- 
    display_full_square(SquareLine, Y), 
    YNext is Y + 1,
    display_board([], SquareBoard, YNext).

display_board([OctagonLine | OctagonBoard], [SquareLine | SquareBoard], Y) :-
    display_full_square(SquareLine, Y), nl,
    write(Y), write('    |   '), display_octagon_line(OctagonLine), nl,
    YNext is Y + 1,
    display_board(OctagonBoard, SquareBoard, YNext).

display_full_square(SquareLine, Y) :-
    write('    '), display_lower_octagon(Y), nl,
    write('     '), display_square_line(SquareLine), nl,
    write('    '), display_upper_octagon(Y).
  
display_square_line([]).
display_square_line([H | []]) :- write(H).
display_square_line([H | T]) :- write(H), write(' ----- '), display_square_line(T).


display_upper_octagon(0) :- write('  /     \\ /     \\ /     \\ /     \\ /     \\ /     \\ /     \\ /     \\ ').
display_upper_octagon(8) :- write('        \\ /     \\ /     \\ /     \\ /     \\ /     \\ /     \\ / ').
display_upper_octagon(_) :- write('\\ /     \\ /     \\ /     \\ /     \\ /     \\ /     \\ /     \\ /     \\ /').
display_lower_octagon(0) :- write('        / \\     / \\     / \\     / \\     / \\     / \\     / \\').
display_lower_octagon(8) :- write('  \\     / \\     / \\     / \\     / \\     / \\     / \\     / \\     / ').
display_lower_octagon(_) :- write('/ \\     / \\     / \\     / \\     / \\     / \\     / \\     / \\     / \\').

display_octagon_line([]).
display_octagon_line([H | T]) :- write(H), write('   |   '), display_octagon_line(T). 
