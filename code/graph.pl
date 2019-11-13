:- use_module(library(ugraphs)).

%   In this file:
% 
% -  build_graph(+BoardState,+Player,-Graph)  

/**
*   build_graph(+BoardState,+Player,-Graph)            
*
*   Builds a players graph according to the Boardstate. This graph represents all of Player's octagon pieces and their connections. BoardState holds the octagonboard, the
*   square board, the board height and the board width.
*/
build_graph([OctagonBoard,SquareBoard,Height,Width],Player,Graph) :- 

    % Build the edges of the graph
    build_edges(OctagonBoard,SquareBoard,Height,Width,Player,GraphEdges), 
    
    % Build the actual graph (from ugraphs library)
    vertices_edges_to_ugraph([],GraphEdges,Graph).


/**
*   build_edges(+OctagonBoard, +SquareBoard, +Height, +Width, +Player, -Edges)
*
*   Build all the edges needed to build the player's graph. This graph represents all of Player's octagon pieces and their connections.
*/
build_edges(OctagonBoard,[_|RemainingSquareBoard],Height,Width,Player,GraphEdges) :-

    build_edges_iter(OctagonBoard,RemainingSquareBoard,Height,Width,UnidirectionalGraphEdges,[],Player,0),

    % Duplicate all the edges (reversing the duplicates orientation) in order to make the graph undirected
    make_undirected(UnidirectionalGraphEdges,GraphEdges).

build_edges_iter([_|[]],_,_,_,GraphEdges,GraphEdges,_,_).

% Build_edges_iter processes the octagon board from top to bottom (row 0 to height - 2).
build_edges_iter([CurrentOctRow,NextOctRow|RemainingOctagonBoard],[NextSqRow|RemainingSquareBoard],Height,Width,GraphEdges,Edges,Player,OctRowY):-

    % Get the paths from the current row
    add_paths_from_row(CurrentOctRow,NextOctRow,NextSqRow,Height,Width,Player,OctRowY,NewEdges),

    % Accumulate the calculated Edges
    append(Edges,NewEdges,NextEdges),

    % Increase the row counter
    NewOctRowY is OctRowY + 1,
    build_edges_iter([NextOctRow|RemainingOctagonBoard],RemainingSquareBoard,Height,Width,GraphEdges,NextEdges,Player,NewOctRowY).

/**
*   add_paths_from_row(+CurrentOctRow,+NextOctRow,+NextSquareRow,+BoardHeight,+BoardWidth, +Player, +OctRowY,-GraphEdges)
*
*   Add the edges that go from octagons in CurrentOctRow to octagons in the same and in NextOctRow, taking into account connections between octagons of CurrentOctRow
*   and the row below that are formed through squares.
*/
add_paths_from_row(CurrentOctRow,NextOctRow,NextSqRow,Height,Width,Player,OctRowY,GraphEdges) :- add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,Height,Width,GraphEdges,Player,[],0,OctRowY).

add_paths_from_row_iter(_,_,_,_,Width,GraphEdges,_,GraphEdges,Width,_).

add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,Height,Width,GraphEdges,Player,Edges,XCoord,YCoord) :- 
    nth0(XCoord,CurrentOctRow,Cell),

    % Cell in positions XCoord YCoord doesn't belong to Player, move on to next cell
    Cell =\= Player,
    NewXCoord is XCoord +1,
    add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,Height,Width,GraphEdges,Player,Edges,NewXCoord,YCoord).   

add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,Height,Width,GraphEdges,Player,Edges,XCoord,YCoord) :- 

    nth0(XCoord,CurrentOctRow,Cell),
    Cell =:= Player,
    NewXCoord is XCoord +1,

    % Check all the possible directions for adjacent octagons with the same color. Below L corresponds to the octagon-cell of the next octagon row that 
    % is to the left, diagonally, of the current Cell. The same goes for below R and below C, that represent the same but for the right diagonal cell and
    % the octagon directly below (center cell). Level L and Level R are the cells that are to the left and right in the same octagon row of Cell.
    get_child_from_belowL(Player,NextOctRow,NextSqRow,Width,EdgeA,XCoord,YCoord),
    get_child_from_belowC(Player,NextOctRow,Width,EdgeB,XCoord,YCoord),
    get_child_from_belowR(Player,NextOctRow,NextSqRow,Width,EdgeC,XCoord,YCoord),
    get_child_from_levelL(Player,CurrentOctRow,Width,EdgeD,XCoord,YCoord),
    get_child_from_levelR(Player,CurrentOctRow,Width,EdgeE,XCoord,YCoord),

    % Append all the results
    append(Edges,EdgeA,Edges1),
    append(Edges1,EdgeB,Edges2),
    append(Edges2,EdgeC,Edges3),
    append(Edges3,EdgeD,Edges4),
    append(Edges4,EdgeE,Edges5),

    add_paths_from_row_iter(CurrentOctRow,NextOctRow,NextSqRow,Height,Width,GraphEdges,Player,Edges5,NewXCoord,YCoord). 

% In the first cell of a row there is no left level child
get_child_from_levelL(_,_,_,[],0,_).

get_child_from_levelL(Player,CurrentOctRow,Width,Edge,XCoord,YCoord) :-
    NewXCoord is XCoord - 1,
    nth0(NewXCoord,CurrentOctRow,Val),
    Val =:= Player,
    CurrCoord is Width * YCoord + XCoord,      
    AdjCoord is CurrCoord - 1,
    Edge = [CurrCoord-AdjCoord].

get_child_from_levelL(Player,CurrentOctRow,_,Edge,XCoord,_) :-
    NewXCoord is XCoord - 1,
    nth0(NewXCoord,CurrentOctRow,Val),
    Val =\= Player,
    Edge = [].

% In the last cell of a row there is no right level child
get_child_from_levelR(_,_,Width,[],W,_) :- W =:= Width - 1.      

get_child_from_levelR(Player,CurrentOctRow,Width,Edge,XCoord,YCoord) :-
    NewXCoord is XCoord + 1,
    nth0(NewXCoord,CurrentOctRow,Val),
    Val =:= Player,
    CurrCoord is Width * YCoord + XCoord,       
    AdjCoord is CurrCoord + 1,
    Edge = [CurrCoord-AdjCoord].

get_child_from_levelR(Player,CurrentOctRow,_,Edge,XCoord,_) :-
    NewXCoord is XCoord + 1,
    nth0(NewXCoord,CurrentOctRow,Val),
    Val =\= Player,
    Edge = [].

get_child_from_belowC(Player,NextOctRow,Width,Edge,XCoord,YCoord) :-
    nth0(XCoord,NextOctRow,Val),
    Val =:= Player,
    AdjCoord is Width * (YCoord + 1) + XCoord,  
    CurrCoord is Width * YCoord + XCoord,       
    Edge = [CurrCoord-AdjCoord].

get_child_from_belowC(Player,NextOctRow,_,Edge,XCoord,_) :-
    nth0(XCoord,NextOctRow,Val),
    Val =\= Player,
    Edge = [].


% In the first cell of a row there is no left below child
get_child_from_belowL(_,_,_,_,[],0,_).

get_child_from_belowL(Player,NextOctRow,NextSqRow,Width,Edge,XCoord,YCoord) :-
    nth0(XCoord,NextSqRow,SqVal),
    SqVal =:= Player,
    AdjXCoord is XCoord - 1,
    nth0(AdjXCoord,NextOctRow,OctVal),
    OctVal =:= Player,
    AdjCoord is Width * (YCoord + 1) + (XCoord - 1),   
    CurrCoord is Width * YCoord + XCoord,       
    Edge = [CurrCoord-AdjCoord].

get_child_from_belowL(Player,NextOctRow,NextSqRow,_,Edge,XCoord,_) :-
    nth0(XCoord,NextSqRow,SqVal),
    SqVal =:= Player,
    AdjXCoord is XCoord - 1,
    nth0(AdjXCoord,NextOctRow,OctVal),
    OctVal =\= Player,
    Edge = [].

get_child_from_belowL(Player,_,NextSqRow,_,Edge,XCoord,_) :-
    nth0(XCoord,NextSqRow,Val),
    Val =\= Player,
    Edge = [].

% In the last cell of a row there is no right below child
get_child_from_belowR(_,_,Width,[],W,_) :- W =:= Width - 1.    

get_child_from_belowR(Player,NextOctRow,NextSqRow,Width,Edge,XCoord,YCoord) :-
    NewXCoord is XCoord +1,
    nth0(NewXCoord,NextSqRow,SqVal),
    SqVal =:= Player,
    AdjXCoord is XCoord + 1,
    nth0(AdjXCoord,NextOctRow,OctVal),
    OctVal =:= Player,
    AdjCoord is Width * (YCoord + 1) + (XCoord + 1),    
    CurrCoord is Width * YCoord + XCoord,              
    Edge = [CurrCoord-AdjCoord].

get_child_from_belowR(Player,NextOctRow,NextSqRow,_,Edge,XCoord,_) :-
    NewXCoord is XCoord +1,
    nth0(NewXCoord,NextSqRow,SqVal),
    SqVal =:= Player,
    AdjXCoord is XCoord + 1,
    nth0(AdjXCoord,NextOctRow,OctVal),
    OctVal =\= Player,
    Edge = [].
        

get_child_from_belowR(Player,_,NextSqRow,_,Edge,XCoord,_) :-
    NewXCoord is XCoord +1,
    nth0(NewXCoord,NextSqRow,Val),
    Val =\= Player,
    Edge = [].

/**
*   make_undirected(+Graph1,-Graph2)
*
*   Take Graph1 and make it undirected, i.e. duplicate all edges reversing the orientation of the duplicates.
*/
make_undirected(Graph1,Graph2) :- make_undirected_iter(Graph1,Graph2,[]).

make_undirected_iter([],Graph,Graph).
make_undirected_iter([A-B|T],Graph,Acc) :- 
    append(Acc,[A-B],Acc1),
    append(Acc1,[B-A],Acc2),
    make_undirected_iter(T,Graph,Acc2).

