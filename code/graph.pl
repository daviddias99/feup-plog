% build_graph(OctagonBoard,SquareBoard,Graph,Color) :- get_valid_starters(OctagonBoard,Color,Starters).



get_valid_starters([H|T],Color,Starters) :- member(Color,H), fecthStarters(H,Color,Starters).

fetch_starters(Row, Color, Result) :- fetch_starters_iter().




% printing aux functions
print_arr([]).
print_arr([H|T]) :- print_row(H),nl,print_arr(T).

print_row([]).
print_row([H|T]) :- write(H), write(','),print_row(T).