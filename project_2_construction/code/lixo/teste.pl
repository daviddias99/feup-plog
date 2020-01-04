:- use_module(library(clpfd)).

teste(Vars) :-

    Vars = [A,B,C,E],
    Lines = [line(A,B),
    line(C,2),
    line(E,0)],

    domain([A,B,C,E],1,3),
    disjoint1(Lines),
    labeling([],Vars).