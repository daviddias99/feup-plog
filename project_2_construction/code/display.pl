display_operations(Operations, WorkersMatrix) :-
    write('id\tespecialidade\tDp\tPc\tOi\tDi\tEi\tHi\tWorkers'), nl,
    write('--\t-------------\t--\t--\t--\t--\t--\t--\t-------'), nl,
    display_operations_aux(Operations, WorkersMatrix).
    
display_operations_aux([], _).
display_operations_aux([[ID, _IDobra, Esp, Dbase, Custo, Oi, Di, Ei, Hi] | RestOps], WorkersMatrix) :-
    write(ID), write('\t'),
    write(Esp), write('\t\t'), write(Dbase), write('\t'),
    write(Custo), write('\t'), write(Oi), write('\t'),
    write(Di), write('\t'), write(Ei), write('\t'), write(Hi), write('\t'), 
    display_operation_workers(ID, WorkersMatrix), nl, nl,
    display_operations_aux(RestOps, WorkersMatrix).


display_constructions([], _, _).
display_constructions([[ID, Payment, Duration, Bonus] | Constructions], Operations, WorkersMatrix) :-
    write('ID\tPayment\tDuration\tBonus'), nl,
    write(ID), write('\t'), write(Payment), write('\t\t'), write(Duration), write('\t\t\t'), write(Bonus), nl,
    findall(Operation, (Operation = [_, ID |_], member(Operation, Operations)), ConstructionOps),
    display_operations(ConstructionOps, WorkersMatrix), nl,
    display_constructions(Constructions, Operations, WorkersMatrix).

display_operation_workers(ID, WorkersMatrix) :-
    nth1(ID, WorkersMatrix, WorkersOperation),
    display_hot_indices(WorkersOperation, 1).

display_hot_indices([], _).
display_hot_indices([1 | T], I) :- write(I), write(' '), I1 is I + 1, display_operation_workers_aux(T, I1).
display_hot_indices([0 | T], I) :- I1 is I + 1, display_operation_workers_aux(T, I1).
    


display_workers(WorkersMatrix) :- 
    transpose(WorkersMatrix, TransposedMatrix),
    display_workers_aux(TransposedMatrix, 1).

display_workers_aux([], _).
display_workers_aux([WorkerRow | WorkersMatrix], I) :-
    write(I), write(': '), display_hot_indices(WorkerRow, 1),
    I1 is I + 1,
    display_workers_aux(WorkersMatrix, I1).
