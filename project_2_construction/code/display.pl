display_operations(Operations) :-
    write('id\tespecialidade\tDp\tPc\tOi\tDi\tEi\tHi'), nl,
    write('--\t-------------\t--\t--\t--\t--\t--\t--'), nl,
    display_operations_aux(Operations).
    
display_operations_aux([]).
display_operations_aux([[ID, _IDobra, Esp, Dbase, Custo, Oi, Di, Ei, Hi] | RestOps]) :-
    write(ID), write('\t'),
    write(Esp), write('\t\t'), write(Dbase), write('\t'),
    write(Custo), write('\t'), write(Oi), write('\t'),
    write(Di), write('\t'), write(Ei), write('\t'),
    write(Hi), nl,
    display_operations_aux(RestOps).


display_constructions([], _).
display_constructions([[ID, Payment, Duration, Bonus] | Constructions], Operations) :-
    write('ID\tPayment\tDuration\tBonus'), nl,
    write(ID), write('\t'), write(Payment), write('\t\t'), write(Duration), write('\t\t\t'), write(Bonus), nl,
    findall(Operation, (Operation = [_, ID |_], member(Operation, Operations)), ConstructionOps),
    display_operations(ConstructionOps), nl,
    display_constructions(Constructions, Operations).

    
    

