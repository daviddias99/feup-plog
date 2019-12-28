:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- [data].

dostuff(Vars) :-
    tell('file.txt'),
    % Fetch variables
    trabalhadores(WorkersI),
    operacoes(OperationsI),
    precedencias(PrecedencesI),
    obras(ConstructionsI),
    especialidades(SpecialtiesI),

    % Init tasks
    length(WorkersI, Nworkers),
    initTasks(OperationsI, Operations, Tasks, Nworkers),
    
    getTaskPrecedences(ConstructionsI, PrecedencesI, Operations, [], CumulativePrecedences),
    
    % Restriction #1
    cumulative(Tasks, [limit(Nworkers), precedences(CumulativePrecedences)]),
    
    % Restriction #2, #3 and #4
    initTasksWorkersMatrix(Operations, WorkersMatrix, WorkersI, Nworkers, SpecialtiesI),

    % Restricton #5
    transpose(WorkersMatrix,TransposedWorkersMatrix),
    imposeNoOverLaps(Tasks,TransposedWorkersMatrix),

    calculateProfit(Tasks,Operations,WorkersI, WorkersMatrix, ConstructionsI,Profit),

    % Solution search
    getVars(Tasks, Vars),
    getPrecendenceVars(Ps,PrecedenceVars),
    flattenMatrix(WorkersMatrix,FlatMatrix,[]),
    append(Vars,FlatMatrix,FinalVars),
    append(FinalVars,PrecedenceVars,FinalFinalVars),
    append(FinalVars,[Profit],FinalFinalFinalVars),
    labeling([maximize(Profit),ffc], FinalFinalFinalVars),
    write(Tasks), write('\n'),write(WorkersMatrix),write('\n'), write(Profit),
    told.

initTasks([], [], [], _).
initTasks([[TaskID, ConstructionID, Specialty, BaseDuration, Cost | _] | OperationsI], [[TaskID, ConstructionID, Specialty, BaseDuration, Cost, Oi, Di, Ei, Hi] | RestOps], [task(Oi, Di, Ei, Hi, TaskID) | RestTasks], Nworkers) :-
    domain([Ei, Oi], 0, 100), % TODO: change this
    Di in 1..BaseDuration,
    Hi in 1..Nworkers,
    Di #= BaseDuration / Hi,
    initTasks(OperationsI, RestOps, RestTasks, Nworkers).

getVars([], []).    
getVars([task(Oi, Di, Ei, Hi, ID) | Tasks], [Oi, Di, Ei, Hi, ID| Vars]) :- getVars(Tasks, Vars). 

getPrecendenceVars([],[]). 

getPrecendenceVars([_-_#=Var|T1],[Var|T2]) :-
    getPrecendenceVars(T1,T2). 

initTasksWorkersMatrix([], [], _, _, _).
initTasksWorkersMatrix( [[_TaskID,_ConstuctionID, Specialty, _BaseDuration,_Cost,_Oi,_Di,_Ei, Hi] | Operations], [NewRow | Matrix], WorkersI, Nworkers, SpecialtiesI) :-
    
    % Restriction #2
    length(NewRow, Nworkers),
    domain(NewRow, 0, 1),
    
    % Restriction #3
    sum(NewRow, #=, Hi),

    % Restriction #4
    atLeastOneSpecialty(NewRow, Specialty, WorkersI, SpecialtiesI),

    initTasksWorkersMatrix(Operations, Matrix, WorkersI, Nworkers, SpecialtiesI).

% At least one specialty

atLeastOneSpecialty(NewRow, Specialty, WorkersI, SpecialtiesI) :-
    createSpecialtyVector(Specialty, SpecialtiesI, Vector),
    specialtyList(Vector, WorkersI, SpecialtyList),
    scalar_product(SpecialtyList, NewRow, #>, 0).

% Get specialty List of a worker

specialtyList(_, [], []).
specialtyList(Specialty, [[_, WorkerSpecialties] | WorkersI], [B | List]) :-
    scalar_product_reif(WorkerSpecialties, Specialty, #>, 0, B),
    specialtyList(Specialty, WorkersI, List).

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

    nth1(Index,Row,Element),
    nth1(Index,Tasks,task(Oi, Di, Ei, Hi, ID)),
    D #= Di * Element,
    D in 0.. 100, % TODO : CHANGE TsHIS LIMIT
    Line = line(Oi,D),
    append(LinesAcc,[Line],NewLinesAcc),
    NewIndex is Index + 1,
    imposeNoOverlap(NewIndex,Tasks,Row,Lines,NewLinesAcc,StoppingIndex).


% Flatten matrix

flattenMatrix([],Result,Result).

flattenMatrix([H|T],Result,Acc) :-
    append(Acc,H,Acc1),
    flattenMatrix(T,Result,Acc1).
    
getTaskPrecedences([], _, _, Result, Result).
getTaskPrecedences([[ConstructionID | _] | ConstructionsI], PrecedencesI, Operations, Acc, Result) :-
    findall(Operacao, (Operacao = [_, ConstructionID | _], member(Operacao, Operations)), ConstructionOps),
    getConstructionTaskPrecedences(PrecedencesI, ConstructionOps, ConstructionPrec),
    restrictPrecedences(Operations, ConstructionPrec),
    append(Acc, ConstructionPrec, Acc1),
    getTaskPrecedences(ConstructionsI, PrecedencesI, Operations, Acc1, Result).

getConstructionTaskPrecedences(Precs, Operations, ConstructionPrec) :-
    findall(IDafter-IDbefore #= Dij, (member(Before-After, Precs), Op1 = [IDbefore, _, Before | _], Op2 = [IDafter, _, After | _], member(Op1, Operations), member(Op2, Operations)), ConstructionPrec).

restrictPrecedences(_, []).
restrictPrecedences(Operations, [IDafter-IDbefore #= Dij | ConstructionPrec]) :-
    nth1(IDbefore, Operations, [_, _, _, _, _, Di |_]),
    Dij #> Di,
    restrictPrecedences(Operations, ConstructionPrec).

createSpecialtyVector(_, [], []).

createSpecialtyVector(Specialty, [Specialty | SpecialtiesI], [1|T]) :- !,
    createSpecialtyVector(Specialty,SpecialtiesI,T).

createSpecialtyVector(Specialty,[_|SpecialtiesI],[0|T]) :- !,
    createSpecialtyVector(Specialty,SpecialtiesI,T).



% Calculate profit

calculateProfit(Tasks, Operations, WorkersI, WorkersMatrix, Constructions, Profit) :-
    getResourceCost(Operations,ResourceCost,0),
    TempProfit2 is 0 - ResourceCost,
    getSalariesCost(Tasks,WorkersI,WorkersMatrix,SalariesCost,0,EndTimes,[]),
    TempProfit3 #= TempProfit2 - SalariesCost,
    getConstructionsPayment(Constructions, Operations, Payment),
    Profit #= TempProfit3 + Payment.

getConstructionsPayment([], _, 0).
getConstructionsPayment([CurrentConstruction | Constructions], Operations, Payment) :-
    computeConstructionPayment(CurrentConstruction, Operations, CurrentPayment),
    getConstructionsPayment(Constructions, Operations, SubPayment),
    Payment #= CurrentPayment + SubPayment.

computeConstructionPayment([ID, Value, ExpectedDuration, BonusFee], Operations, Payment) :-
    getConstructionStartTimes(ID, Operations, StartTimes),
    minimum(Start, StartTimes),
    getConstructionEndTimes(ID, Operations, EndTimes),
    maximum(End, EndTimes),
    domain([Duration, End, Start], 0, 100),
    Duration #= End - Start,
    Payment #= Value + BonusFee * (ExpectedDuration - Duration).


getConstructionStartTimes(ConstructionID, [], []).
getConstructionStartTimes(ConstructionID, [[_, ConstructionID, _, _, _, Start | _] | Operations], [Start | StartTimes]) :- !,    
    getConstructionStartTimes(ConstructionID, Operations, StartTimes).
getConstructionStartTimes(ConstructionID, [_ | Operations], StartTimes) :-
    getConstructionStartTimes(ConstructionID, Operations, StartTimes).


getConstructionEndTimes(ConstructionID, [], []).
getConstructionEndTimes(ConstructionID, [[_, ConstructionID, _, _, _, _, _, End | _] | Operations], [End | EndTimes]) :- !,    
    getConstructionEndTimes(ConstructionID, Operations, EndTimes).
getConstructionEndTimes(ConstructionID, [_ | Operations], EndTimes) :-
    getConstructionEndTimes(ConstructionID, Operations, EndTimes).

getResourceCost([],ResourceCost,ResourceCost).

getResourceCost([Task|Tasks],ResourceCost,Acc) :-

    Task = [_TaskID, _ConstructionID, _Specialty, _BaseTime, Cost | _],
    Acc1 is Acc + Cost,
    getResourceCost(T,ResourceCost,Acc1). 

getSalariesCost([],_,_,SalariesCost,SalariesCost,EndTimes,EndTimes).
getSalariesCost([task(Oi, Di, Ei, Hi, ID)|Tasks], WorkersI, [TaskWorkers|WorkersMatrix],SalariesCost,SalaryAcc,EndTimes,EndTimesAcc) :-
    getTaskSalaryCost(Di,WorkersI,TaskWorkers,TaskSalary,0,1),
    SalaryAcc1 #= SalaryAcc + TaskSalary,
    append(EndTimesAcc,[Ei],EndTimesAcc1),
    getSalariesCost(Tasks,WorkersI,WorkersMatrix,SalariesCost,SalaryAcc1,EndTimes,EndTimesAcc1).


getTaskSalaryCost(_,_,[],TaskSalary,TaskSalary,_).

getTaskSalaryCost(Di,WorkersI, [H|T], TaskSalary, Acc, Index) :-

    nth1(Index, WorkersI,[WorkerSalary, _]),
    Acc1 #= Acc + WorkerSalary * Di * H,
    NewIndex is Index + 1,
    getTaskSalaryCost(Di,WorkersI,T,TaskSalary,Acc1,NewIndex).

