:- [display].

start(Player) :- init_board(Board), display_game(Board, Player).

init_board([[
             [0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 2, 0, 0, 0, 0, 0],
             [0, 1, 2, 0, 1, 0, 0, 0],
             [0, 2, 0, 0, 2, 0, 0, 0],
             [0, 0, 1, 0, 1, 0, 0, 0],
             [0, 0, 0, 0, 1, 0, 2, 0],
             [0, 0, 0, 0, 0, 0, 1, 0],
             [0, 0, 0, 0, 0, 0, 0, 0]
            ],
            [
             [0, 1, 1, 1, 1, 1, 1, 1, 0],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 1, 0, 0, 2],
             [2, 0, 1, 0, 1, 0, 2, 0, 2],
             [2, 0, 0, 0, 2, 0, 0, 0, 2],
             [2, 0, 2, 0, 0, 0, 0, 0, 2],
             [2, 0, 0, 1, 0, 0, 0, 0, 2],
             [2, 0, 0, 0, 0, 0, 0, 0, 2],
             [0, 1, 1, 1, 1, 1, 1, 1, 0]
            ]]).

