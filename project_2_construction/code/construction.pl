:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- [data].

dostuff(Vars) :-
    tell('file.txt'),
    % Fetch variables
    trabalhadores(Workers),
    operacoes(Ops),
    precedencias(Prec),
    obra([Custo, Duracao, Bonus]),

    % Init tasks
    length(Workers, Nworkers),
    initTasks(Ops, Tasks, Nworkers),

    % Restriction #1
    cumulative(Tasks, [limit(Nworkers)]),
    
    % Restriction #2, #3 and #4
    initTasksWorkersMatrix(Ops, Tasks, Matrix, Workers, Nworkers),

    % Resticton #5
    transpose(Matrix,TransposedMatrix),
    imposeNoOverLaps(Tasks,TransposedMatrix),

    calculateProfit(Tasks,Ops,Workers,Matrix,[Custo, Duracao, Bonus],Profit),

    % Solution search
    getVars(Tasks, Vars),
    flattenMatrix(Matrix,FlatMatrix,[]),
    append(Vars,FlatMatrix,FinalVars),
    append(FinalVars,[Profit],FinalFinalVars),
    labeling([maximize(Profit)], FinalFinalVars),
    write(Tasks), write('\n'),write(Matrix),write('\n'), write(Profit),
    told.

initTasks([], [], _).
initTasks([[ID, _IDobra, _Esp, Dbase | _] | RestOps], [task(Oi, Di, Ei, Hi, ID) | RestTasks], Nworkers) :-
    domain([Ei, Oi], 0, 100),
    Di in 1..Dbase,
    Hi in 1..Nworkers,
    Di #= Dbase / Hi,
    initTasks(RestOps, RestTasks, Nworkers).

getVars([], []).    
getVars([task(Oi, Di, Ei, Hi, ID) | Tasks], [Oi, Di, Ei, Hi, ID| Vars]) :- getVars(Tasks, Vars). 

initTasksWorkersMatrix([], [], [], _, _).
initTasksWorkersMatrix([[_ID, Esp, _Dbase, _Custo] | Ops], [task(_Oi, _Di, _Ei, Hi, _ID) | Tasks], [NewRow | Matrix], Workers, Nworkers) :-
    
    % Restriction #2
    length(NewRow, Nworkers),
    domain(NewRow, 0, 1),
    
    % Restriction #3
    sum(NewRow, #=, Hi),

    % Restriction #4
    atLeastOneSpecialty(NewRow, Esp, Workers),

    initTasksWorkersMatrix(Ops, Tasks, Matrix, Workers, Nworkers).

% At least one specialty

atLeastOneSpecialty(NewRow, Specialty, Workers) :-
    specialtyList(Specialty, Workers, SpecialtyList),
    scalar_product(SpecialtyList, NewRow, #>, 0).

% Get specialty List of a worker

specialtyList(_, [], []).
specialtyList(Specialty, [[_, WorkerSpecialties] | Workers], [B | List]) :-
    scalar_product_reif(WorkerSpecialties, Specialty, #>, 0, B),
    specialtyList(Specialty, Workers, List).

% Impose no overlaps

imposeNoOverLaps(_,[]).
imposeNoOverLaps(Tasks,[Row|TransposedMatrix]) :-
    length(Row,Length),
    StoppingIndex is Length + 1,
    imposeNoOverlap(1,Tasks,Row,Lines,[],StoppingIndex),
    disjoint1(Lines),
    imposeNoOverLaps(Tasks,TransposedMatrix).

% Impose no overlap

imposeNoOverlap(StoppingIndex,_,_,Lines,Lines,StoppingIndex).
imposeNoOverlap(Index,Tasks,Row,Lines,LinesAcc,StoppingIndex) :-

    element(Index,Row,1),
    nth1(Index,Tasks,task(Oi, Di, Ei, Hi, ID)),
    Line = line(Oi,Di),
    append(LinesAcc,[Line],NewLinesAcc),
    NewIndex is Index + 1,
    imposeNoOverlap(NewIndex,Tasks,Row,Lines,NewLinesAcc,StoppingIndex).

imposeNoOverlap(Index,Tasks,Row,Lines,LinesAcc,StoppingIndex) :-

    element(Index,Row,0),
    NewIndex is Index + 1,
    imposeNoOverlap(NewIndex,Tasks,Row,Lines,LinesAcc,StoppingIndex).

% Flatten matrix

flattenMatrix([],Result,Result).

flattenMatrix([H|T],Result,Acc) :-
    append(Acc,H,Acc1),
    flattenMatrix(T,Result,Acc1).


% Calculate profit

calculateProfit(Tasks, Ops, Workers,WorkersMatrix, [Custo, Duracao, Bonus], Profit) :-

    TempProfit1 is Custo,
    getResourceCost(Ops,ResourceCost,0),
    TempProfit2 is TempProfit1 - ResourceCost,
    getSalariesCost(Tasks,Workers,WorkersMatrix,SalariesCost,0,EndTimes,[]),
    TempProfit3 #= TempProfit2 - SalariesCost,
    maximum(Max,EndTimes),
    Profit #= TempProfit3 + Bonus * (Duracao - Max).


getResourceCost([],ResourceCost,ResourceCost).

getResourceCost([H|T],ResourceCost,Acc) :-

    H = [_ID, _Spec, _BaseTime, Cost],
    Acc1 is Acc + Cost,
    getResourceCost(T,ResourceCost,Acc1). 

getSalariesCost([],_,_,SalariesCost,SalariesCost,EndTimes,EndTimes).
getSalariesCost([task(Oi, Di, Ei, Hi, ID)|T], Workers, [TaskWorkers|WorkersMatrix],SalariesCost,SalaryAcc,EndTimes,EndTimesAcc) :-
    getTaskSalaryCost(Di,Workers,TaskWorkers,TaskSalary,0,1),
    SalaryAcc1 #= SalaryAcc + TaskSalary,
    append(EndTimesAcc,[Ei],EndTimesAcc1),
    getSalariesCost(T,Workers,WorkersMatrix,SalariesCost,SalaryAcc1,EndTimes,EndTimesAcc1).


getTaskSalaryCost(_,_,[],TaskSalary,TaskSalary,_).

getTaskSalaryCost(Di,Workers, [1|T], TaskSalary, Acc, Index) :-

    nth1(Index, Workers,[WorkerSalary, _]),
    Acc1 #= Acc + WorkerSalary * Di,
    NewIndex is Index + 1,
    getTaskSalaryCost(Di,Workers,T,TaskSalary,Acc1,NewIndex).

getTaskSalaryCost(Di,Workers, [0|T], TaskSalary, Acc, Index) :-

    NewIndex is Index + 1,
    getTaskSalaryCost(Di,Workers,T, TaskSalary,Acc,NewIndex).