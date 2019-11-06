init_board([[
             [0, 0, 0, 1, 2, 2, 2, 2],
             [0, 0, 0, 1, 1, 1, 1, 1],
             [0, 0, 2, 2, 1, 1, 2, 1],
             [0, 2, 2, 1, 2, 1, 2, 1],
             [0, 0, 1, 2, 1, 2, 2, 1],
             [0, 0, 0, 0, 2, 1, 0, 1],
             [0, 0, 0, 0, 1, 2, 1, 0],
             [0, 0, 0, 0, 1, 1, 2, 2]
            ],
            [
             [0, 1, 1, 1, 1, 1, 1, 1, 0],
             [2, 0, 0, 0, 1, 0, 0, 0, 2],
             [2, 0, 0, 0, 1, 1, 1, 1, 2],
             [2, 0, 0, 0, 0, 1, 0, 0, 2],
             [2, 0, 0, 1, 1, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 1, 0, 0, 2],
             [2, 0, 0, 0, 0, 1, 1, 1, 2],
             [2, 0, 0, 0, 0, 1, 0, 0, 2],
             [0, 1, 1, 1, 1, 1, 1, 1, 0]
            ]]
).

test():- init_board(Board),teste(Board).



teste([H1,H2|_]) :- build_graph(H1,H2,Graph,1),print_arr(Graph).


% add paths to graph

build_graph(OctagonBoard,SquareBoard,Graph,Color) :-
        build_graph_iter(OctagonBoard,SquareBoard,Graph,[],Color,0).

build_graph_iter([_|[]],_,Graph,Graph,_,_).

build_graph_iter([CurrentOctRow,NextOctRow|RemainingOctagonBoard],[NextSqRow|RemainingSquareBoard],Graph,Edges,Color,OctRowY):-
        add_paths_from_row(CurrentOctRow,NextOctRow,NextSqRow,NewEdges,Color,OctRowY),
        append(Edges,NewEdges,NextEdges),
        NewOctRowY is OctRowY + 1,
        build_graph_iter([NextOctRow|RemainingOctagonBoard],RemainingSquareBoard,Graph,NextEdges,Color,NewOctRowY).

add_paths_from_row(CurrentOctRow,NextOctRow,NextSqRow,Graph,Color,OctRowY) :- add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,Graph,Color,[],1,OctRowY).

add_paths_from_row_iter(_,_,_,Graph,_,Graph,7,_).

add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,Graph,Color,Edges,XCoord,YCoord) :- 
        nth0(XCoord,CurrentOctRow,Cell),
        Cell =\= Color,
        NewXCoord is XCoord +1,
        add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,Graph,Color,Edges,NewXCoord,YCoord).   

add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,Graph,Color,Edges,XCoord,YCoord) :- 

        nth0(XCoord,CurrentOctRow,Cell),
        Cell =:= Color,
        NewXCoord is XCoord +1,

        get_child_from_belowL(Color,NextSqRow,EdgeA,XCoord,YCoord),
        get_child_from_belowC(Color,NextOctRow,EdgeB,XCoord,YCoord),
        get_child_from_belowR(Color,NextSqRow,EdgeC,XCoord,YCoord),
        get_child_from_levelL(Color,NextOctRow,EdgeD,XCoord,YCoord),
        get_child_from_levelR(Color,NextOctRow,EdgeE,XCoord,YCoord),

        append(Edges,EdgeA,Edges1),
        append(Edges1,EdgeB,Edges2),
        append(Edges2,EdgeC,Edges3),
        append(Edges3,EdgeD,Edges4),
        append(Edges4,EdgeE,Edges5),

        add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,Graph,Color,Edges5,NewXCoord,YCoord). 

get_child_from_levelL(Color,CurrentOctRow,Edge,XCoord,YCoord) :-
        NewXCoord is XCoord - 1,
        nth0(NewXCoord,CurrentOctRow,Val),
        Val =:= Color,
        CurrCoord is 8 * YCoord + XCoord,
        AdjCoord is CurrCoord - 1,
        Edge = [CurrCoord-AdjCoord].

get_child_from_levelL(Color,CurrentOctRow,Edge,XCoord,_) :-
        NewXCoord is XCoord - 1,
        nth0(NewXCoord,CurrentOctRow,Val),
        Val =\= Color,
        Edge = [].

get_child_from_levelR(Color,CurrentOctRow,Edge,XCoord,YCoord) :-
        NewXCoord is XCoord + 1,
        nth0(NewXCoord,CurrentOctRow,Val),
        Val =:= Color,
        CurrCoord is 8 * YCoord + XCoord,
        AdjCoord is CurrCoord + 1,
        Edge = [CurrCoord-AdjCoord].

get_child_from_levelR(Color,CurrentOctRow,Edge,XCoord,_) :-
        NewXCoord is XCoord + 1,
        nth0(NewXCoord,CurrentOctRow,Val),
        Val =\= Color,
        Edge = [].


get_child_from_belowL(Color,NextSqRow,Edge,XCoord,YCoord) :-
        nth0(XCoord,NextSqRow,Val),
        Val =:= Color,
        AdjCoord is 8 * (YCoord + 1) + (XCoord - 1),
        CurrCoord is 8 * YCoord + XCoord,
        Edge = [CurrCoord-AdjCoord].

get_child_from_belowL(Color,NextSqRow,Edge,XCoord,_) :-
        nth0(XCoord,NextSqRow,Val),
        Val =\= Color,
        Edge = [].


get_child_from_belowC(Color,NextOctRow,Edge,XCoord,YCoord) :-
        nth0(XCoord,NextOctRow,Val),
        Val =:= Color,
        AdjCoord is 8 * (YCoord + 1) + XCoord,
        CurrCoord is 8 * YCoord + XCoord,
        Edge = [CurrCoord-AdjCoord].

get_child_from_belowC(Color,NextOctRow,Edge,XCoord,_) :-
        nth0(XCoord,NextOctRow,Val),
        Val =\= Color,
        Edge = [].

get_child_from_belowR(Color,NextSqRow,Edge,XCoord,YCoord) :-
        NewXCoord is XCoord +1,
        nth0(NewXCoord,NextSqRow,Val),
        Val =:= Color,
        AdjCoord is 8 * (YCoord + 1) + (XCoord + 1),
        CurrCoord is 8 * YCoord + XCoord,
        Edge = [CurrCoord-AdjCoord].

get_child_from_belowR(Color,NextSqRow,Edge,XCoord,_) :-
        NewXCoord is XCoord +1,
        nth0(NewXCoord,NextSqRow,Val),
        Val =\= Color,
        Edge = [].

              





































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