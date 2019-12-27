:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- [data].

dostuff(Vars) :-
    tell('file.txt'),
    % Fetch variables
    trabalhadores(Workers),
    operacoes(Ops),
    precedencias(Prec),
    obras(Obras),
    especialidades(Specialties),

    % Init tasks
    length(Workers, Nworkers),
    initTasks(Ops, Operations, Tasks, Nworkers),
    getTaskPrecedences(Obras, Prec, Operations, [], Ps),

    % Restriction #1
    cumulative(Tasks, [limit(Nworkers), precedences(Ps)]),
    
    % Restriction #2, #3 and #4
    initTasksWorkersMatrix(Ops, Tasks, Matrix, Workers, Nworkers, Specialties),

    % Resticton #5
    transpose(Matrix,TransposedMatrix),
    imposeNoOverLaps(Tasks,TransposedMatrix),

    %calculateProfit(Tasks,Ops,Workers,Matrix,[Custo, Duracao, Bonus],Profit),

    % Solution search
    getVars(Tasks, Vars),
    flattenMatrix(Matrix,FlatMatrix,[]),
    append(Vars,FlatMatrix,FinalVars),
    %append(FinalVars,[Profit],FinalFinalVars),
    labeling([], FinalVars),
    write(Tasks), write('\n'),write(Matrix),write('\n'),% write(Profit),
    told.

initTasks([], [], [], _).
initTasks([[ID, IDobra, Esp, Dbase | _] | RestInput], [[ID, IDobra, Esp, Dbase, Oi, Di, Ei, Hi] | RestOps], [task(Oi, Di, Ei, Hi, ID) | RestTasks], Nworkers) :-
    domain([Ei, Oi], 0, 100),
    Di in 1..Dbase,
    Hi in 1..Nworkers,
    Di #= Dbase / Hi,
    initTasks(RestInput, RestOps, RestTasks, Nworkers).

getVars([], []).    
getVars([task(Oi, Di, Ei, Hi, ID) | Tasks], [Oi, Di, Ei, Hi, ID| Vars]) :- getVars(Tasks, Vars). 

initTasksWorkersMatrix([], [], [], _, _, _).
initTasksWorkersMatrix([[_ID, _IDobra, Esp, _Dbase, _Custo] | Ops], [task(_Oi, _Di, _Ei, Hi, _ID) | Tasks], [NewRow | Matrix], Workers, Nworkers, Specialties) :-
    
    % Restriction #2
    length(NewRow, Nworkers),
    domain(NewRow, 0, 1),
    
    % Restriction #3
    sum(NewRow, #=, Hi),

    % Restriction #4
    atLeastOneSpecialty(NewRow, Esp, Workers, Specialties),

    initTasksWorkersMatrix(Ops, Tasks, Matrix, Workers, Nworkers, Specialties).

% At least one specialty

atLeastOneSpecialty(NewRow, Specialty, Workers, Specialties) :-
    createSpecialtyVector(Specialty, Specialties, Vector),
    specialtyList(Vector, Workers, SpecialtyList),
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


    
getTaskPrecedences([], _, _, Result, Result).
getTaskPrecedences([[IDobra | _] | Obras], Prec, Ops, Acc, Result) :-
    findall(Operacao, (Operacao = [_, IDobra | _], member(Operacao, Ops)), ConstructionOps),
    getConstructionTaskPrecedences(Prec, ConstructionOps, ConstructionPrec),
    append(Acc, ConstructionPrec, Acc1),
    getTaskPrecedences(Obras, Prec, Ops, Acc1, Result).

getConstructionTaskPrecedences(Precs, Ops, ConstructionPrec) :-
    findall(IDafter-IDbefore #= Dij, (member(Before-After, Precs), Op1 = [IDbefore, _, Before, _, _, Di | _], Op2 = [IDafter, _, After | _], member(Op1, Ops), member(Op2, Ops), Dij #>= Di), ConstructionPrec).

createSpecialtyVector(_, [], []).

createSpecialtyVector(Specialty, [Specialty | Specialties], [1|T]) :- !,
    createSpecialtyVector(Specialty,Specialties,T).

createSpecialtyVector(Specialty,[_|Specialties],[0|T]) :- !,
    createSpecialtyVector(Specialty,Specialties,T).
