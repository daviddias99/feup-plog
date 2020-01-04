display_operations(Operations, WorkersMatrix) :-
    write('\tID\tSpecialty\t\tBD\tMC\tStart\tDur\tEnd\tWorkers'), nl,
    write('\t--\t---------\t\t--\t--\t-----\t---\t---\t-------'), nl,
    display_operations_aux(Operations, WorkersMatrix).
    
display_operations_aux([], _).
display_operations_aux([[ID, _IDobra, Esp, Dbase, Custo, Oi, Di, Ei, _] | RestOps], WorkersMatrix) :-
    write('\t'), write(ID), write('\t'),
    write(Esp), write('\t\t'), write(Dbase), write('\t'),
    write(Custo), write('\t'), write(Oi), write('\t\t'),
    write(Di), write('\t'), write(Ei), write('\t'), 
    display_operation_workers(ID, WorkersMatrix), nl, 
    display_operations_aux(RestOps, WorkersMatrix).


display_constructions_([], _, _).
display_constructions_([[ID, BP, CD, Bonus] | Constructions], Operations, WorkersMatrix) :-
    construction_duration_payment([ID, BP, CD, Bonus], Operations, Duration, Payment),
    write('---------------------------------------------------'), nl,
    write('ID\t\tBP\t\tCD\t\tBonus\tDuration\tPayment'), nl,
    write(ID), write('\t\t'), write(BP), write('\t\t'), write(CD), write('\t\t'), write(Bonus), write('\t\t'), write(Duration), write('\t\t\t'), write(Payment), nl, nl,
    findall(Operation, (Operation = [_, ID |_], member(Operation, Operations)), ConstructionOps),
    display_operations(ConstructionOps, WorkersMatrix), nl,
    display_constructions_(Constructions, Operations, WorkersMatrix).

display_constructions(Constructions, Operations, WorkersMatrix) :-
    write(' ---------------'), nl,
    write('| Constructions |'), nl,
    write(' ---------------'), nl, nl,
    write('BP: base price, CD: contract duration, BD: base duration, MC: materials cost'), nl,
    display_constructions_(Constructions, Operations, WorkersMatrix).

display_operation_workers(ID, WorkersMatrix) :-
    nth1(ID, WorkersMatrix, WorkersOperation),
    display_hot_indices(WorkersOperation, 1).

display_hot_indices([], _).
display_hot_indices([1 | T], I) :- write(I), write(' '), I1 is I + 1, display_hot_indices(T, I1).
display_hot_indices([0 | T], I) :- I1 is I + 1, display_hot_indices(T, I1).

display_list([H]) :- write(H), !.
display_list([H | T]) :-
    write(H), write(', '), display_list(T).

display_workers(WorkersMatrix, Workers) :-
    write(' --------- '), nl,
    write('| Workers |'), nl, 
    write(' --------- '), nl, nl,
    write('ID\tSallary\tOperations'), nl,
    write('--\t-------\t----------'), nl,
    transpose(WorkersMatrix, TransposedMatrix),
    display_workers_aux(TransposedMatrix, 1, Workers).

display_workers_aux([], _, _).
display_workers_aux([WorkerRow | WorkersMatrix], I, WorkersI) :-
    nth1(I, WorkersI, [S | _]),
    write(I), write('\t'), write(S), write('\t\t'), display_hot_indices(WorkerRow, 1), nl,
    I1 is I + 1,
    display_workers_aux(WorkersMatrix, I1, WorkersI).

display_profit(Profit) :-
    write('Profit: '), write(Profit).

display_labeling_vars(Vars) :-
    write('Labeling vars: '),
    display_list(Vars).


construction_duration_payment([ID, Value, ExpectedDuration, BonusFee], Operations, Duration, Payment) :-
    construction_start_times(ID, Operations, StartTimes),
    construction_end_times(ID, Operations, EndTimes),
    min_member(Start, StartTimes),
    max_member(End, EndTimes),
    Duration is End - Start,
    Payment is Value + BonusFee * (ExpectedDuration - Duration).
    

construction_start_times(_, [], []).
construction_start_times(ConstructionID, [[_, ConstructionID, _, _, _, Start | _] | Operations], [Start | StartTimes]) :- !,    
    construction_start_times(ConstructionID, Operations, StartTimes).
construction_start_times(ConstructionID, [_ | Operations], StartTimes) :-
    construction_start_times(ConstructionID, Operations, StartTimes).

construction_end_times(_, [], []).
construction_end_times(ConstructionID, [[_, ConstructionID, _, _, _, _, _, End | _] | Operations], [End | EndTimes]) :- !,    
    construction_end_times(ConstructionID, Operations, EndTimes).
construction_end_times(ConstructionID, [_ | Operations], EndTimes) :-
    construction_end_times(ConstructionID, Operations, EndTimes).
