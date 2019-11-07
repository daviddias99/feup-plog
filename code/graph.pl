:- use_module(library(ugraphs)).

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

test(G):- init_board(Board),teste(Board,G).



teste([H1,H2|_],G) :- build_edges(H1,H2,GraphEdges,1), vertices_edges_to_ugraph([],GraphEdges,G).


% get the graphs edges

build_edges(OctagonBoard,[_|RemainingSquareBoard],GraphEdges,Color) :-
        build_edges_iter(OctagonBoard,RemainingSquareBoard,GraphEdges,[],Color,0).

build_edges_iter([_|[]],_,GraphEdges,GraphEdges,_,_).

build_edges_iter([CurrentOctRow,NextOctRow|RemainingOctagonBoard],[NextSqRow|RemainingSquareBoard],GraphEdges,Edges,Color,OctRowY):-
        add_paths_from_row(CurrentOctRow,NextOctRow,NextSqRow,NewEdges,Color,OctRowY),
        append(Edges,NewEdges,NextEdges),
        NewOctRowY is OctRowY + 1,
        build_edges_iter([NextOctRow|RemainingOctagonBoard],RemainingSquareBoard,GraphEdges,NextEdges,Color,NewOctRowY).

add_paths_from_row(CurrentOctRow,NextOctRow,NextSqRow,GraphEdges,Color,OctRowY) :- add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,GraphEdges,Color,[],0,OctRowY).

add_paths_from_row_iter(_,_,_,GraphEdges,_,GraphEdges,8,_).

add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,GraphEdges,Color,Edges,XCoord,YCoord) :- 
        nth0(XCoord,CurrentOctRow,Cell),
        Cell =\= Color,
        NewXCoord is XCoord +1,
        add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,GraphEdges,Color,Edges,NewXCoord,YCoord).   

add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,GraphEdges,Color,Edges,XCoord,YCoord) :- 

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

        add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,GraphEdges,Color,Edges5,NewXCoord,YCoord). 

get_child_from_levelL(_,_,[],0,_).

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

get_child_from_levelR(_,_,[],7,_).

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

get_child_from_belowL(_,_,[],0,_).

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

get_child_from_belowR(_,_,[],7,_).

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