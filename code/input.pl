input_menu_option(Option) :-
    get_int_between(' > Choose game mode ', Option, 0, 4).

input_board_size(Height, Width) :-
    ansi_format([bold], ' > Choose board size ', [world]), nl,
    ansi_format([bold], '\t> Height ', [world]), get_int(Height),
    ansi_format([bold], '\t> Width ', [world]), get_int(Width).
    
input_bot_level(Level) :-
    get_int_between(' > CPU level ', Level, 1, 2).

input_bot_level(Level, 1) :-
    get_int_between(' > CPU 1 level ', Level, 1, 2).

input_bot_level(Level, 2) :-
    get_int_between(' > CPU 2 level ', Level, 1, 2).


input_move(X-Y, Height, Width) :- 
    ansi_format([bold], ' > Choose move ', [world]), nl,
    input_hor_coord(X, Width), 
    input_ver_coord(Y, Height).

input_ver_coord(Coord, Height) :- 
    MaxCoord is Height - 1,
    get_int_between('\t> Vertical coordinate ', Coord, 0, MaxCoord).

input_hor_coord(Coord, Width) :- 
    repeat,
    ansi_format([bold], '\t> Horizontal coordinate ', [world]), 
    get_letter(Letter),
    letter_to_int(Letter, Coord),
    Coord >= 0, Coord =< Width - 1.


get_int_between(Prompt, Option, Min, Max) :- 
    repeat, 
    ansi_format([bold], Prompt, [world]), 
    get_int(Option), 
    Option >= Min, Option =< Max.

get_letter(Letter) :- get_char_list(InputList), InputList \== [], char_list_to_letter(InputList, Letter).

char_list_to_letter(['e', 'x', 'i', 't'], _) :- abort. 
char_list_to_letter([Letter | []], Letter).

letter_to_int(Letter, Int) :- atom_codes(Letter, Code), Code >= 97, Code =< 122, Int is Code - 97. 

flush :- get_char('\n').
flush :- flush_aux, !, fail.
flush_aux :- repeat, get_char(C), C == '\n'.

get_int(Int) :- get_char_list(InputList), InputList \== [], char_list_to_int(InputList, Int).

get_char_list([]) :- peek_char('\n'), !, get_char(_).
get_char_list([Input | InputList]) :- get_char(Input), get_char_list(InputList).

char_list_to_int(['e', 'x', 'i', 't'], 0) :- abort.
char_list_to_int(DigitList, Int) :- char_list_to_int_aux(DigitList, 0, Int).

char_list_to_int_aux([], Int, Int).
char_list_to_int_aux([Digit | DigitList], Int, Res) :- 
    atom_number(Digit, Num), 
    NewInt is Int * 10 + Num, 
    char_list_to_int_aux(DigitList, NewInt, Res).

press_enter_to_continue :-
    ansi_format([bold], 'Press ', [world]),
    ansi_format([bold, fg(green)], '<ENTER>', [world]),
    ansi_format([bold], ' to continue.', [world]),
    wait_for_enter, nl, nl.

wait_for_enter :- get_char('\n'), !.
wait_for_enter :- get_char(_), wait_for_enter. 