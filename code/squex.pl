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

display_board([OctagonBoard, SquareBoard |[]]) :- display_board(OctagonBoard, SquareBoard).

display_board([],[]).
display_board([], [SquareLine | SquareBoard]) :- 
    display_full_square(SquareLine), 
    display_board([], SquareBoard).
display_board([OctagonLine | OctagonBoard], [SquareLine | SquareBoard]) :-
    display_full_square(SquareLine), nl,
    write('|   '), display_octagon_line(OctagonLine), nl,
    display_board(OctagonBoard, SquareBoard).

display_full_square(SquareLine) :-
    display_lower_octagon, nl,
    write(' '), display_square_line(SquareLine), nl,
    display_upper_octagon.

display_square_line([]).
display_square_line([H | []]) :- write(H).
display_square_line([H | T]) :- write(H), write(' --- '), display_square_line(T).

display_upper_octagon :- write('\\ /   \\ /   \\ /   \\ /   \\ /   \\ /   \\ /   \\ /   \\ /').
display_lower_octagon :- write('/ \\   / \\   / \\   / \\   / \\   / \\   / \\   / \\   / \\').

display_octagon_line([]).
display_octagon_line([H | T]) :- write(H), write('  |  '), display_octagon_line(T). 
