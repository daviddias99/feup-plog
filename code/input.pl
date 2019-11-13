input_menu_option(Option) :-
    get_int_between(' > Choose game mode ', Option, 0, 4).

input_board_size(Width, Height) :-
    ansi_format([bold], ' > Choose board size ', [world]), nl,
    ansi_format([bold], '\t> Width ', [world]), get_int(Width),
    ansi_format([bold], '\t> Height ', [world]), get_int(Height).
    
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

get_letter(_) :- peek_char('\n'), !, get_char(_), fail.
get_letter(Letter) :- get_char(Letter), flush.

letter_to_int(Letter, Int) :- atom_codes(Letter, Code), Code >= 97, Code =< 122, Int is Code - 97. 
letter_to_int(Letter, Int) :- atom_codes(Letter, Code), Code >= 65, Code >= 90, Int is Code - 65.

flush :- get_char('\n').
flush :- flush_aux, !, fail.
flush_aux :- repeat, get_char(C), C == '\n'.

get_int(Int) :- get_int_aux(InputList), InputList \== [], digit_list_to_int(InputList, Int).

get_int_aux([]) :- peek_char('\n'), !, get_char(_).
get_int_aux([Input | InputList]) :- get_char(Input), get_int_aux(InputList).

digit_list_to_int(DigitList, Int) :- digit_list_to_int_aux(DigitList, 0, Int).

digit_list_to_int_aux([], Int, Int).
digit_list_to_int_aux([Digit | DigitList], Int, Res) :- 
    atom_number(Digit, Num), 
    NewInt is Int * 10 + Num, 
    digit_list_to_int_aux(DigitList, NewInt, Res).

press_enter_to_continue :-
    ansi_format([bold], 'Press ', [world]),
    ansi_format([bold, fg(green)], '<ENTER>', [world]),
    ansi_format([bold], ' to continue.', [world]),
    wait_for_enter.

wait_for_enter :- get_char('\n'), !.
wait_for_enter :- get_char(_), wait_for_enter. 