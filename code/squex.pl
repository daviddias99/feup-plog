:- [display], [input], [generate], [gameover], [game_logic].

init_board([[
             [0, 0, 0, 1, 0, 0, 2, 0],
             [0, 0, 0, 1, 2, 2, 0, 2],
             [2, 2, 2, 2, 1, 2, 2, 1],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0]
            ],
            [
             [0, 1, 1, 1, 1, 1, 1, 1, 0],
             [2, 0, 0, 0, 0, 0, 2, 2, 2],
             [2, 0, 0, 0, 2, 2, 2, 2, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [0, 1, 1, 1, 1, 1, 1, 1, 0]
            ],
            8,
            8]
).

play :-
    display_main_screen,
    input_menu_option(Option),
    repeat, input_board_size(Height, Width), generate_board(Height, Width, Board), !,
    start_game(Option, Board).

start_game(1, Board) :-
    game_loop(Board, 1).

game_loop(Board, Player) :-
    Board = [_, _, Height, Width | []],
    display_game(Board, Player), nl, nl,
    repeat,
    input_move(X, Y, Height, Width),
    move(Player, X-Y, Board, NewBoard, _), !,
    continue_game(NewBoard, Player).

continue_game(NewBoard, Player) :-
    gameover(NewBoard, Player), !, write(Player), write(' won'), nl.

continue_game(NewBoard, Player) :-
    get_other_player(Player, NextPlayer),
    game_loop(NewBoard, NextPlayer).


