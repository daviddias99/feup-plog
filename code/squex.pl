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

%
%
game_loop(GameState) :-
    display_game(GameState), nl, nl,
    repeat,
    get_move(Move, GameState),
    move(Move, GameState, NewGameState), !,
    continue_game(NewGameState).


%continue_game(NewGameState) :-
    % gameover(NewGameState, Player), !, display_gameover(NewGameState, Player).

continue_game(NewGameState) :-
    game_loop(NewGameState).

get_move(Move, GameState) :-
    GameState = [_, _, _, _, _, _, Player | _ ],
    CurrPlayerTypeIndex is 3 + Player,
    nth0(CurrPlayerTypeIndex, GameState, CurrPlayerType),
    get_move_aux(Move, GameState, CurrPlayerType).

get_move_aux(Move, GameState, 'P') :-
    GameState = [_, _, Width, Height, _, _, _, _| []],
    input_move(Move, Height, Width).


