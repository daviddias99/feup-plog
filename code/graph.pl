:- use_module(library(ugraphs)).
% build_graph(+OctagonBoard,+SquareBoard,+Player,-Graph)
build_graph(OctagonBoard,SquareBoard,Player,Graph) :- build_edges(OctagonBoard,SquareBoard,GraphEdges,Player), vertices_edges_to_ugraph([],GraphEdges,Graph).

% get the graphs edges

build_edges(OctagonBoard,[_|RemainingSquareBoard],GraphEdges,Color) :-
        build_edges_iter(OctagonBoard,RemainingSquareBoard,GraphEdges1,[],Color,0),
        make_undirected(GraphEdges1,GraphEdges).

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

        get_child_from_belowL(Color,NextOctRow,NextSqRow,EdgeA,XCoord,YCoord),
        get_child_from_belowC(Color,NextOctRow,EdgeB,XCoord,YCoord),
        get_child_from_belowR(Color,NextOctRow,NextSqRow,EdgeC,XCoord,YCoord),
        get_child_from_levelL(Color,CurrentOctRow,EdgeD,XCoord,YCoord),
        get_child_from_levelR(Color,CurrentOctRow,EdgeE,XCoord,YCoord),

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


get_child_from_belowL(_,_,_,[],0,_).

get_child_from_belowL(Color,NextOctRow,NextSqRow,Edge,XCoord,YCoord) :-
        nth0(XCoord,NextSqRow,SqVal),
        SqVal =:= Color,
        AdjXCoord is XCoord - 1,
        nth0(AdjXCoord,NextOctRow,OctVal),
        OctVal =:= Color,
        AdjCoord is 8 * (YCoord + 1) + (XCoord - 1),
        CurrCoord is 8 * YCoord + XCoord,
        Edge = [CurrCoord-AdjCoord].

get_child_from_belowL(Color,NextOctRow,NextSqRow,Edge,XCoord,_) :-
        nth0(XCoord,NextSqRow,SqVal),
        SqVal =:= Color,
        AdjXCoord is XCoord - 1,
        nth0(AdjXCoord,NextOctRow,OctVal),
        OctVal =\= Color,
        Edge = [].

get_child_from_belowL(Color,_,NextSqRow,Edge,XCoord,_) :-
        nth0(XCoord,NextSqRow,Val),
        Val =\= Color,
        Edge = [].

get_child_from_belowR(_,_,_,[],7,_).

get_child_from_belowR(Color,NextOctRow,NextSqRow,Edge,XCoord,YCoord) :-
        NewXCoord is XCoord +1,
        nth0(NewXCoord,NextSqRow,SqVal),
        SqVal =:= Color,
        AdjXCoord is XCoord + 1,
        nth0(AdjXCoord,NextOctRow,OctVal),
        OctVal =:= Color,
        AdjCoord is 8 * (YCoord + 1) + (XCoord + 1),
        CurrCoord is 8 * YCoord + XCoord,
        Edge = [CurrCoord-AdjCoord].

get_child_from_belowR(Color,NextOctRow,NextSqRow,Edge,XCoord,_) :-
        NewXCoord is XCoord +1,
        nth0(NewXCoord,NextSqRow,SqVal),
        SqVal =:= Color,
        AdjXCoord is XCoord + 1,
        nth0(AdjXCoord,NextOctRow,OctVal),
        OctVal =\= Color,
        Edge = [].
        

get_child_from_belowR(Color,_,NextSqRow,Edge,XCoord,_) :-
        NewXCoord is XCoord +1,
        nth0(NewXCoord,NextSqRow,Val),
        Val =\= Color,
        Edge = [].

make_undirected(Graph1,Graph2) :- make_undirected_iter(Graph1,Graph2,[]).

make_undirected_iter([],Graph,Graph).
make_undirected_iter([A-B|T],Graph,Acc) :- 
        append(Acc,[A-B],Acc1),
        append(Acc1,[B-A],Acc2),
        make_undirected_iter(T,Graph,Acc2).

