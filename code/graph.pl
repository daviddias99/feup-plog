init_board([[
             [0, 0, 0, 1, 2, 2, 2, 2],
             [0, 0, 0, 1, 1, 1, 1, 1]
            ],
            [
             [0, 1, 1, 1, 1, 1, 1, 1, 0],
             [2, 0, 0, 0, 1, 0, 0, 0, 2],
             [2, 0, 0, 0, 1, 1, 1, 1, 2]
            ]]
).

test():- init_board(Board),teste(Board).



teste([H1,H2|_]) :- build_graph(H1,H2,Graph,1),print_arr(Graph).




% add paths to graph

build_graph([_|[]],_,[],_).
build_graph([CurrentOctRow,NextOctRow|RemainingOctagonBoard],[NextSqRow|RemainingSquareBoard],[H|Graph],Color) :-
        add_paths_from_row(CurrentOctRow,NextOctRow,NextSqRow,H,Color),
        build_graph([NextOctRow|RemainingOctagonBoard],RemainingSquareBoard,Graph,Color).

add_paths_from_row(CurrentOctRow,NextOctRow,NextSqRow,Graph,Color) :- add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,Graph,Color,[],1,0).

add_paths_from_row_iter([],_,_,Graph,_,Graph,_,_).

add_paths_from_row_iter([Cell|T],NextOctRow,NextSqRow,Graph,Color,Edges,XCoord,YCoord) :- 
        Cell =\= Color, 
        NewXCoord is XCoord +1,
        add_paths_from_row_iter(T,NextOctRow,NextSqRow,Graph,Color,Edges,NewXCoord,YCoord).


add_paths_from_row_iter([Cell|T],NextOctRow,NextSqRow,Graph,Color,Edges,XCoord,YCoord) :- 
        Cell =:= Color, 
        NewXCoord is XCoord +1,
        nth0(XCoord,NextSqRow,Val),
        Val =:= Color,
        AdjCoord is 8 * (YCoord + 1) + (XCoord - 1),
        CurrCoord is 8 * YCoord + XCoord,
        append(Edges,[[CurrCoord-AdjCoord]],NewEdges),
        add_paths_from_row_iter(T,NextOctRow,NextSqRow,Graph,Color,NewEdges,NewXCoord,YCoord).

add_paths_from_row_iter([Cell|T],NextOctRow,NextSqRow,Graph,Color,Edges,XCoord,YCoord) :- 
        Cell =:= Color, 
        NewXCoord is XCoord +1,
        nth0(NewXCoord,NextSqRow,Val),
        Val =:= Color,
        AdjCoord =is 8 * (YCoord + 1) + (XCoord + 1),
        CurrCoord is 8 * YCoord + XCoord,
        append(Edges,[[CurrCoord-AdjCoord]],NewEdges),
        add_paths_from_row_iter(T,NextOctRow,NextSqRow,Graph,Color,NewEdges,NewXCoord,YCoord).

add_paths_from_row_iter([Cell|T],NextOctRow,NextSqRow,Graph,Color,Edges,XCoord,YCoord) :- 
        Cell =:= Color, 
        NewXCoord is XCoord +1,
        nth0(XCoord,NextOctRow,Val),
        Val =:= Color,
        AdjCoord is 8 * (YCoord + 1) + XCoord,
        CurrCoord is 8 * YCoord + XCoord,
        append(Edges,[[CurrCoord-AdjCoord]],NewEdges),
        add_paths_from_row_iter(T,NextOctRow,NextSqRow,Graph,Color,NewEdges,NewXCoord,YCoord).






































% get in Starters a list with the indexes of the valid starting points
get_valid_starters([H|_],Color,Starters) :- member(Color,H), fecthStarters(H,Color,Starters).

fetch_starters(Row, Color, Result) :- fetch_starters_iter(Row,Color,Result,[],0).

fetch_starters_iter([],_,Result,Result,_).
fetch_starters_iter([H|T],Color,Result,Acc,N) :-  
        H =:= Color, 
        append(Acc,[N],Acc1),
        N1 is N + 1, 
        fetch_starters_iter(T,Color,Result,Acc1,N1).

fetch_starters_iter([H|T],Color,Result,Acc,N) :-  
        H =\= Color, 
        N1 is N + 1, 
        fetch_starters_iter(T,Color,Result,Acc,N1).

% printing aux functions
print_arr(H) :- print_row(H).
print_arr([]).
print_arr([H|T]) :- print_row(H),nl,print_arr(T).

print_row([]).
print_row([H|T]) :- write(H), write(','),print_row(T).