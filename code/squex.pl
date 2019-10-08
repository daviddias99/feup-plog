
    % [
    %     [0,0,0,0,0,0,0,0],
    %     [0,0,0,0,0,0,0,0],
    %     [0,0,0,0,0,0,0,0],
    %     [0,0,0,0,0,0,0,0],
    %     [0,0,0,0,0,0,0,0],
    %     [0,0,0,0,0,0,0,0],
    %     [0,0,0,0,0,0,0,0],
    %     [0,0,0,0,0,0,0,0]
    % ],[
    %     [6,1,1,1,1,1,1,6],
    %     [2,0,0,0,0,0,0,2],
    %     [2,0,0,0,0,0,0,2],
    %     [2,0,0,0,0,0,0,2],
    %     [2,0,0,0,0,0,0,2],
    %     [2,0,0,0,0,0,0,2],
    %     [2,0,0,0,0,0,0,2],
    %     [2,0,0,0,0,0,0,2],
    %     [6,1,1,1,1,1,1,6]
    % ]).

display_board([],[]).
display_board([],[Line2 | Board2]) :- display_line(Line2), nl,display_board([],Board2).
display_board([Line1 | Board1],[Line2 | Board2]) :- display_line(Line2), nl, display_line(Line1), nl,display_board(Board1,Board2).

display_line([]).
display_line([H | T]) :- write(H), display_line(T).