:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- [data].

dostuff(Vars1) :-
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
    imposeNoOverLaps(Tasks,WorkersMatrix),

    % Optimization function
    calculateProfit(Tasks,Operations,WorkersI, WorkersMatrix, ConstructionsI,Profit),

    % Solution search
    getVars(Tasks, Vars1),
    flattenMatrix(WorkersMatrix,FlatMatrix,[]),
    append(Vars1,FlatMatrix,Vars2),
    append(Vars2,[Profit],Vars3),
    labeling([maximize(Profit),ffc], Vars3),
    write(Tasks), write('\n'),write(WorkersMatrix),write('\n'), write(Profit),
    told.

% Task initialization

initTasks([], [], [], _).
initTasks([[TaskID, ConstructionID, Specialty, BaseDuration, Cost | _] | OperationsI], [[TaskID, ConstructionID, Specialty, BaseDuration, Cost, Oi, Di, Ei, Hi] | RestOps], [task(Oi, Di, Ei, Hi, TaskID) | RestTasks], Nworkers) :-
    domain([Ei, Oi], 0, 100), % TODO: domain needs improvement
    Di in 1..BaseDuration,
    Hi in 1..Nworkers,
    Di #= BaseDuration / Hi,
    initTasks(OperationsI, RestOps, RestTasks, Nworkers).

% --- Worker Schedule initialization
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

% --- Assure that there is a worker assigned to a task that has it's speciality
atLeastOneSpecialty(NewRow, Specialty, WorkersI, SpecialtiesI) :-

    % Create a vector that has as one in the position corresponding to Specialty
    createSpecialtyVector(Specialty, SpecialtiesI, Vector),

    % Create a vector that has a 1 in a position in which the worker corresponding to that position has Specialty and 0 otherwise
    specialtyList(Vector, WorkersI, SpecialtyList),
    
    % Ensure that there is one person with that specialty
    scalar_product(SpecialtyList, NewRow, #>, 0).

% --- Create a vector that has a 1 in a position in which the worker corresponding to that position has Specialty
specialtyList(_, [], []).
specialtyList(Specialty, [[_, WorkerSpecialties] | WorkersI], [B | List]) :-
    scalar_product_reif(WorkerSpecialties, Specialty, #>, 0, B),
    specialtyList(Specialty, WorkersI, List).

% --- Create a vector that has as one in the position corresponding to Specialty
createSpecialtyVector(_, [], []).

createSpecialtyVector(Specialty, [Specialty | SpecialtiesI], [1|T]) :- !,
    createSpecialtyVector(Specialty,SpecialtiesI,T).

createSpecialtyVector(Specialty,[_|SpecialtiesI],[0|T]) :- !,
    createSpecialtyVector(Specialty,SpecialtiesI,T).

% --- Impose that a worker can't be working in to tasks at the same time
imposeNoOverLaps(Tasks, WorkersMatrix) :-
    transpose(WorkersMatrix,TransposedWorkersMatrix),
    TransposedWorkersMatrix = [SampleRow | _],
    length(SampleRow, RowLength),
    StoppingIndex is RowLength + 1,
    imposeNoOverLaps_aux(Tasks,TransposedWorkersMatrix,StoppingIndex).

imposeNoOverLaps_aux(_,[],_).
imposeNoOverLaps_aux(Tasks,[Row|TransposedMatrix],StoppingIndex) :-

    imposeNoOverlap(1,Tasks,Row,Lines,[],StoppingIndex),
    disjoint1(Lines),
    imposeNoOverLaps_aux(Tasks,TransposedMatrix,StoppingIndex).

% --- Impose that a worker can't be working in to tasks at the same time

imposeNoOverlap(StoppingIndex,_,_,Lines,Lines,StoppingIndex).
imposeNoOverlap(Index,Tasks,Row,Lines,LinesAcc,StoppingIndex) :-

    nth1(Index,Row,Element),
    nth1(Index,Tasks,task(Oi, Di, _Ei, _Hi, _ID)),
    D #= Di * Element,
    D in 0.. 100, % TODO: CHANGE THIS LIMIT
    Line = line(Oi,D),
    append(LinesAcc,[Line],NewLinesAcc),
    NewIndex is Index + 1,
    imposeNoOverlap(NewIndex,Tasks,Row,Lines,NewLinesAcc,StoppingIndex).

% -- Get the existing task precedences in a way that is valid for the cumulative restriction    
getTaskPrecedences([], _, _, Result, Result).
getTaskPrecedences([[ConstructionID | _] | ConstructionsI], PrecedencesI, Operations, Acc, Result) :-

    % Get all the operations of the current construction
    findall(Operation, (Operation = [_, ConstructionID | _], member(Operation, Operations)), ConstructionOps),

    % Get the formated residences according to task IDs
    getConstructionTaskPrecedences(PrecedencesI, ConstructionOps, ConstructionPrec),
    
    % Assure that the precedence values are following the right order
    restrictPrecedences(Operations, ConstructionPrec),

    append(Acc, ConstructionPrec, Acc1),
    getTaskPrecedences(ConstructionsI, PrecedencesI, Operations, Acc1, Result).

% --- Get the formated residences according to task IDs
getConstructionTaskPrecedences(Precs, Operations, ConstructionPrec) :-
    findall(IDafter-IDbefore #= _, (member(Before-After, Precs), Op1 = [IDbefore, _, Before | _], Op2 = [IDafter, _, After | _], member(Op1, Operations), member(Op2, Operations)), ConstructionPrec).

% --- Assure that the precedence values are following the right order
restrictPrecedences(_, []).
restrictPrecedences(Operations, [_-IDbefore #= Dij | ConstructionPrec]) :-
    nth1(IDbefore, Operations, [_, _, _, _, _, Di |_]),
    Dij #> Di,
    restrictPrecedences(Operations, ConstructionPrec).

% --- Profit calculation

calculateProfit(Tasks, Operations, WorkersI, WorkersMatrix, Constructions, Profit) :-

    % Get the cost of the resources (this cost must be subtracted from the total)
    getResourceCost(Operations,ResourceCost,0),
    TempProfit1 is 0 - ResourceCost,

    % Get the cost related to worker salaries (this cost must be subtracted from the total)
    getSalariesCost(Tasks,WorkersI,WorkersMatrix,SalariesCost,0),
    TempProfit2 #= TempProfit1 - SalariesCost,

    % Get the payment value from the constructions (including bonuses)
    getConstructionsPayment(Constructions, Operations, Payment),
    Profit #= TempProfit2 + Payment.

% --- Get the cost of the resources of all tasks
getResourceCost([],ResourceCost,ResourceCost).

getResourceCost([Task|Tasks],ResourceCost,Acc) :-

    Task = [_TaskID, _ConstructionID, _Specialty, _BaseTime, Cost | _],
    Acc1 is Acc + Cost,
    getResourceCost(T,ResourceCost,Acc1). 

% --- Get the cost associated with the workers' salary
getSalariesCost([],_,_,SalariesCost,SalariesCost).
getSalariesCost([task(Oi, Di, Ei, Hi, ID)|Tasks], WorkersI, [TaskWorkers|WorkersMatrix],SalariesCost,SalaryAcc) :-
    getTaskSalaryCost(Di,WorkersI,TaskWorkers,TaskSalaryCost,0,1),
    SalaryAcc1 #= SalaryAcc + TaskSalaryCost,
    getSalariesCost(Tasks,WorkersI,WorkersMatrix,SalariesCost,SalaryAcc1).

% --- Get the salary costs associated with a task
getTaskSalaryCost(_,_,[],TaskSalaryCost,TaskSalaryCost,_).
getTaskSalaryCost(Di,WorkersI, [H|T], TaskSalaryCost, Acc, Index) :-

    nth1(Index, WorkersI,[WorkerSalary, _]),
    Acc1 #= Acc + WorkerSalary * Di * H,
    NewIndex is Index + 1,
    getTaskSalaryCost(Di,WorkersI,T,TaskSalaryCost,Acc1,NewIndex).

% --- Get the payment value from the constructions (including bonuses)
getConstructionsPayment([], _, 0).
getConstructionsPayment([CurrentConstruction | Constructions], Operations, Payment) :-
    computeConstructionPayment(CurrentConstruction, Operations, CurrentPayment),
    getConstructionsPayment(Constructions, Operations, SubPayment),
    Payment #= CurrentPayment + SubPayment.

% --- Get the payment value from a single construction (including bonuses)
computeConstructionPayment([ID, Value, ExpectedDuration, BonusFee], Operations, Payment) :-
    getConstructionStartTimes(ID, Operations, StartTimes),
    getConstructionEndTimes(ID, Operations, EndTimes),
    minimum(Start, StartTimes),
    maximum(End, EndTimes),
    domain([Duration, End, Start], 0, 100), % TODO: Change this domain
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

%--------------------%                    
% Auxilary Functions %
%--------------------% 

getVars([], []).    
getVars([task(Oi, Di, Ei, Hi, ID) | Tasks], [Oi, Di, Ei, Hi, ID| Vars]) :- getVars(Tasks, Vars). 

getPrecendenceVars([],[]). 

getPrecendenceVars([_-_#=Var|T1],[Var|T2]) :-
    getPrecendenceVars(T1,T2). 

flattenMatrix([],Result,Result).

flattenMatrix([H|T],Result,Acc) :-
    append(Acc,H,Acc1),
    flattenMatrix(T,Result,Acc1).