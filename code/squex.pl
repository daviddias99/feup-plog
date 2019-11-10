:- [display], [input], [generate], [gameover], [game_logic].

play :-
    display_main_screen,
    input_menu_option(_Option),
    repeat, input_board_size(Height, Width), generate_board(Height, Width, Board), !,
    display_game(Board, 1).
