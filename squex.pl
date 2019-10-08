display_board([]).
display_board([Line | Board]) :- display_line(Line), nl, display_board(Board).

display_line([]).
display_line([H | T]) :- write(H), display_line(T).