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
display_board([], [SquareLine | Squares]) :- 
    display_square_line(SquareLine), 
    display_board([], Squares).
display_board([OctagonLine | Octagons], [SquareLine | Squares]) :-
    display_square_line(SquareLine), nl,
    display_full_octagon(OctagonLine), nl,
    display_board(Octagons, Squares).

display_upper_octagon :- write(' /   \\ /   \\ /   \\ /   \\ /   \\ /   \\ /   \\ /   \\ ').
display_lower_octagon :- write(' \\   / \\   / \\   / \\   / \\   / \\   / \\   / \\   /').

display_full_octagon(OctagonLine) :-
    display_upper_octagon, nl,
    write('|  '), display_octagon_line(OctagonLine), nl,
    display_lower_octagon.

display_octagon_line([]).
display_octagon_line([H | T]) :- write(H), write('  |  '), display_octagon_line(T). 

display_square_line([]).
display_square_line([H | T]) :- write(H), write(' --- '), display_square_line(T).


