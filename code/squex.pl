display() :- display_board([
                            [0,0,0,0,0,0,0,0],
                            [0,0,0,0,0,0,0,0],
                            [0,0,0,0,0,0,0,0],
                            [0,0,0,0,0,0,0,0],
                            [0,0,0,0,0,0,0,0],
                            [0,0,0,0,0,0,0,0],
                            [0,0,0,0,0,0,0,0],
                            [0,0,0,0,0,0,0,0]
                           ],[
                            [' ',1,1,1,1,1,1,1,' '],
                            [2,0,0,0,0,0,0,0,2],
                            [2,0,0,0,0,0,0,0,2],
                            [2,0,0,0,0,0,0,0,2],
                            [2,0,0,0,0,0,0,0,2],
                            [2,0,0,0,0,0,0,0,2],
                            [2,0,0,0,0,0,0,0,2],
                            [2,0,0,0,0,0,0,0,2],
                            [' ',1,1,1,1,1,1,1,' ']
                           ]).

display_board([],[]).
display_board([], [SquareLine | Squares]) :- 
    display_square_line(SquareLine), 
    display_board([], Squares).
display_board([OctagonLine | Octagons], [SquareLine | Squares]) :-
    display_square_line(SquareLine), nl,
    display_upper_octagon_line, nl,
    display_full_octagon_line(OctagonLine), nl,
    display_lower_octagon_line, nl,
    display_board(Octagons, Squares).

display_upper_octagon_line :- write(' /   \\ /   \\ /   \\ /   \\ /   \\ /   \\ /   \\ /   \\ ').
display_lower_octagon_line :- write(' \\   / \\   / \\   / \\   / \\   / \\   / \\   / \\   /').

display_full_octagon_line(OctagonLine) :- write('|  '), display_octagon_line(OctagonLine).

display_octagon_line([]).
display_octagon_line([H | T]) :- write(H), write('  |  '), display_octagon_line(T). 

display_square_line([]).
display_square_line([H | T]) :- write(H), write(' --- '), display_square_line(T).

