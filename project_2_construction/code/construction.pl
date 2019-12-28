:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- [data].

dostuff(Vars) :-
    tell('file.txt'),
    % Fetch variables
    trabalhadores(WorkersI),
    operacoes(OperationsI),
    precedencias(PrecedencesI),
    obras(Obras),
    especialidades(Specialties),

    % Init tasks
    length(WorkersI, Nworkers),
    initTasks(OperationsI, Operations, Tasks, Nworkers),
    
    getTaskPrecedences(Obras, PrecedencesI, Operations, [], Ps),
    
    getPrecendenceVars(Ps,PrecedenceVars),
    

    % Restriction #1
    cumulative(Tasks, [limit(Nworkers), precedences(Ps)]),
    print(Ps),
    
    % Restriction #2, #3 and #4
    initTasksWorkersMatrix(Ops, Tasks, Matrix, WorkersI, Nworkers, Specialties),

    % Resticton #5
    transpose(Matrix,TransposedMatrix),
    imposeNoOverLaps(Tasks,TransposedMatrix),

    calculateProfit(Tasks,Operations,WorkersI,Matrix, Obras,Profit),

    % Solution search
    getVars(Tasks, Vars),
    flattenMatrix(Matrix,FlatMatrix,[]),
    append(Vars,FlatMatrix,FinalVars),
    append(FinalVars,PrecedenceVars,FinalFinalVars),
    append(FinalVars,[Profit],FinalFinalFinalVars),
    labeling([maximize(Profit),ffc], FinalFinalFinalVars),
    write(Tasks), write('\n'),write(Matrix),write('\n'), write(Profit),
    told.

initTasks([], [], [], _).
initTasks([[ID, IDobra, Esp, Dbase, Custo | _] | OperationsI], [[ID, IDobra, Esp, Dbase, Custo, Oi, Di, Ei, Hi] | RestOps], [task(Oi, Di, Ei, Hi, ID) | RestTasks], Nworkers) :-
    domain([Ei, Oi], 0, 100),
    Di in 1..Dbase,
    Hi in 1..Nworkers,
    Di #= Dbase / Hi,
    initTasks(OperationsI, RestOps, RestTasks, Nworkers).

getVars([], []).    
getVars([task(Oi, Di, Ei, Hi, ID) | Tasks], [Oi, Di, Ei, Hi, ID| Vars]) :- getVars(Tasks, Vars). 

getPrecendenceVars([],[]). 

getPrecendenceVars([_-_#=Var|T1],[Var|T2]) :-
    getPrecendenceVars(T1,T2). 

initTasksWorkersMatrix([], [], [], _, _, _).
initTasksWorkersMatrix([[_ID, _IDobra, Esp, _Dbase, _Custo] | OperationsI], [task(_Oi, _Di, _Ei, Hi, _ID) | Tasks], [NewRow | Matrix], WorkersI, Nworkers, Specialties) :-
    
    % Restriction #2
    length(NewRow, Nworkers),
    domain(NewRow, 0, 1),
    
    % Restriction #3
    sum(NewRow, #=, Hi),

    % Restriction #4
    atLeastOneSpecialty(NewRow, Esp, WorkersI, Specialties),

    initTasksWorkersMatrix(OperationsI, Tasks, Matrix, WorkersI, Nworkers, Specialties).

% At least one specialty

atLeastOneSpecialty(NewRow, Specialty, WorkersI, Specialties) :-
    createSpecialtyVector(Specialty, Specialties, Vector),
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
getTaskPrecedences([[IDobra | _] | Obras], PrecedencesI, Ops, Acc, Result) :-
    findall(Operacao, (Operacao = [_, IDobra | _], member(Operacao, Ops)), ConstructionOps),
    getConstructionTaskPrecedences(PrecedencesI, ConstructionOps, ConstructionPrec),
    restrictPrecedences(Ops, ConstructionPrec),
    append(Acc, ConstructionPrec, Acc1),
    getTaskPrecedences(Obras, PrecedencesI, Ops, Acc1, Result).

getConstructionTaskPrecedences(Precs, Ops, ConstructionPrec) :-
    findall(IDafter-IDbefore #= Dij, (member(Before-After, Precs), Op1 = [IDbefore, _, Before | _], Op2 = [IDafter, _, After | _], member(Op1, Ops), member(Op2, Ops)), ConstructionPrec).

restrictPrecedences(_, []).
restrictPrecedences(Ops, [IDafter-IDbefore #= Dij | ConstructionPrec]) :-
    nth1(IDbefore, Ops, [_, _, _, _, _, Di |_]),
    Dij #> Di,
    restrictPrecedences(Ops, ConstructionPrec).

createSpecialtyVector(_, [], []).

createSpecialtyVector(Specialty, [Specialty | Specialties], [1|T]) :- !,
    createSpecialtyVector(Specialty,Specialties,T).

createSpecialtyVector(Specialty,[_|Specialties],[0|T]) :- !,
    createSpecialtyVector(Specialty,Specialties,T).



% Calculate profit

calculateProfit(Tasks, Ops, WorkersI,WorkersMatrix, Constructions, Profit) :-
    getResourceCost(Ops,ResourceCost,0),
    TempProfit2 is 0 - ResourceCost,
    getSalariesCost(Tasks,WorkersI,WorkersMatrix,SalariesCost,0,EndTimes,[]),
    TempProfit3 #= TempProfit2 - SalariesCost,
    getConstructionsPayment(Constructions, Ops, Payment),
    Profit #= TempProfit3 + Payment.

getConstructionsPayment([], _, 0).
getConstructionsPayment([CurrentConstruction | Constructions], Ops, Payment) :-
    computeConstructionPayment(CurrentConstruction, Ops, CurrentPayment),
    getConstructionsPayment(Constructions, Ops, SubPayment),
    Payment #= CurrentPayment + SubPayment.

computeConstructionPayment([ID, Value, ExpectedDuration, BonusFee], Ops, Payment) :-
    getConstructionStartTimes(ID, Ops, StartTimes),
    minimum(Start, StartTimes),
    getConstructionEndTimes(ID, Ops, EndTimes),
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

getResourceCost([H|T],ResourceCost,Acc) :-

    H = [_ID, _ConstuctionID, _Spec, _BaseTime, Cost | _],
    Acc1 is Acc + Cost,
    getResourceCost(T,ResourceCost,Acc1). 

getSalariesCost([],_,_,SalariesCost,SalariesCost,EndTimes,EndTimes).
getSalariesCost([task(Oi, Di, Ei, Hi, ID)|T], WorkersI, [TaskWorkers|WorkersMatrix],SalariesCost,SalaryAcc,EndTimes,EndTimesAcc) :-
    getTaskSalaryCost(Di,WorkersI,TaskWorkers,TaskSalary,0,1),
    SalaryAcc1 #= SalaryAcc + TaskSalary,
    append(EndTimesAcc,[Ei],EndTimesAcc1),
    getSalariesCost(T,WorkersI,WorkersMatrix,SalariesCost,SalaryAcc1,EndTimes,EndTimesAcc1).


getTaskSalaryCost(_,_,[],TaskSalary,TaskSalary,_).

getTaskSalaryCost(Di,WorkersI, [H|T], TaskSalary, Acc, Index) :-

    nth1(Index, WorkersI,[WorkerSalary, _]),
    Acc1 #= Acc + WorkerSalary * Di * H,
    NewIndex is Index + 1,
    getTaskSalaryCost(Di,WorkersI,T,TaskSalary,Acc1,NewIndex).

