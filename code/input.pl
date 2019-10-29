get_input(Prompt, Input) :- write(' - '), write(Prompt), write(' : '), get_char(Input), flush, !.

flush :- get_char('\n').
flush :- flush_aux, !, fail.

flush_aux :- repeat, get_char(C), C == '\n'.

get_input_ver_coord(Coord) :- repeat, get_input('Vertical coordinate', Input), char_to_ver_coord(Input, Coord).

char_to_ver_coord(Coord, Int) :- char_code(Coord, Code), digit_to_int(Code, Int), Int >= 0, Int =< 7.

digit_to_int(Digit, Int) :- Int is Digit - 48.
letter_to_int(Letter, Int) :- Int is Letter - 97. 

get_input_hor_coord(Coord) :- repeat, get_input('Horizontal coordinate', Input), char_to_hor_coord(Input, Coord).

char_to_hor_coord(Coord, Int) :- char_code(Coord, Code), letter_to_int(Code, Int), Int >= 0, Int =< 7.


get_input_coords(Hor, Ver) :- get_input_hor_coord(Hor), get_input_ver_coord(Ver).