:- [display], [input], [game_model], [game_logic], [gameover], [bot]. 

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
    get_players_type(Option, P1Type, P2Type),
    repeat, input_board_size(Height, Width), generate_initial_game_state(Height, Width, P1Type, P2Type, GameState), !,
    game_loop(GameState).

get_players_type(1, 'P', 'P').
get_players_type(2, 'P', P2Type) :-
    input_bot_level(P2Type).
get_players_type(3, P1Type, 'P') :-
    input_bot_level(P1Type).
get_players_type(4, P1Type, P2Type) :-
    input_bot_level(P1Type, 1),
    input_bot_level(P2Type, 2).

%game_loop(NewGameState) :-
    % gameover(NewGameState, Player), !, display_gameover(NewGameState, Player).

game_loop(GameState) :-
    display_game(GameState), 
    repeat, get_move(GameState, Move), move(Move, GameState, NewGameState), !,
    game_loop(NewGameState).

get_move(GameState, Move) :-
    get_game_current_player_type(GameState, Type),
    choose_move(GameState, Type, Move).

choose_move(GameState, 'P', Move) :-
    get_game_board_size(GameState, Height, Width),
    input_move(Move, Height, Width).

choose_move(GameState, 1, Move) :-
    random_move(GameState, Move),
    write('CHOSEN MOVE: '), write(Move), nl,
    press_enter_to_continue.


