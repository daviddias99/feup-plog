:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- [display].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WARNING : Consult below the file that contains the input of the problem %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- consult('input_files/data.pl').
% :- consult('input_files/mediumdata.pl').
:- consult('input_files/bigdata.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Solver : timeout represents the number of miliseconds to timeout
solve_problem :-
    
    tell('result_files/file.txt'),

    % Fetch variables
    trabalhadores(WorkersI),
    operacoes(OperationsI),
    precedencias(PrecedencesI),
    obras(ConstructionsI),
    especialidades(SpecialtiesI),
    reset_timer,

    % Init tasks(domain variables) and restrict the domains
    length(WorkersI, Nworkers),
    getTaskBaseDurations(OperationsI,Durations),
    sumlist(Durations,TotalDuration),
    initTasks(OperationsI, Operations, Tasks, Nworkers,TotalDuration),
    getTaskPrecedences(ConstructionsI, PrecedencesI, Operations, [], CumulativePrecedences),

    % Apply the cumulative global restriction
    cumulative(Tasks, [limit(Nworkers), precedences(CumulativePrecedences)]),
    
    % Initialize the worker scheduling matrix
    initTasksWorkersMatrix(Operations, WorkersMatrix, WorkersI, Nworkers, SpecialtiesI),

    % Ensure that a worker can't be working at two operations at the same time
    imposeNoOverLaps(Operations,WorkersMatrix),

    % Optimization function
    calculateProfit(Tasks,Operations,WorkersI, WorkersMatrix, ConstructionsI,Profit),

    print_time('PostingConstraints: '),

    % Variable grouping for labeling
    flattenMatrix(WorkersMatrix,FlatMatrix,[]),
    splitTaskVars(Tasks,Origins,Dur,Ends,WorkerCounts),
    append(Dur,FlatMatrix,VarsSet1),
    append(Origins,Ends,VarsSet2),
    append(WorkerCounts,[Profit],VarsSet3),

    % Solution searching
    solve( [maximize(Profit),time_out(600000,_)],
        [
            labeling([leftmost,up],VarsSet1),
            labeling([occurrence,enum,up],VarsSet2),
            labeling([],VarsSet3)
        ]
    ),

    % Solution display
    print_time('LabelingTime: '),
    fd_statistics,
    statistics,
    write(Tasks), write('\n'),write(WorkersMatrix),write('\n'), write(Profit), nl, nl, nl,
    display_profit(Profit), nl,
    nl, display_constructions(ConstructionsI, Operations, WorkersMatrix), display_workers(WorkersMatrix, WorkersI),
    told.


% Task initialization, domain variable definition.

initTasks([], [], [], _,_).
initTasks([[TaskID, ConstructionID, Specialty, BaseDuration, Cost | _] | OperationsI], [[TaskID, ConstructionID, Specialty, BaseDuration, Cost, Oi, Di, Ei, Hi] | RestOps], [task(Oi, Di, Ei, Hi, TaskID) | RestTasks], Nworkers,TotalDuration) :-
    
    % The start and end points of a task must be at most, aprox., the sum of the base durations of all tasks
    domain([Ei, Oi], 0, TotalDuration), 
    
    % The end of a task must be before it's start
    Ei #> Oi,

    % The ending instant of a task must not exceed it's start instant plus it's baseduration
    Ei #=< Oi + BaseDuration,

    % The value of the actual duration of a task must never exceed it's base duration nor can it be lower than one day
    Di in 1..BaseDuration,
    
    % At most the number of workers assigned to a task can be the total number of workers
    Hi in 1..Nworkers,
    Hi #=< BaseDuration,

    % The actual duration of a task is calculated by dividing it's base time with the number of workers assigned to it
    Di #= BaseDuration / Hi,
    initTasks(OperationsI, RestOps, RestTasks, Nworkers,TotalDuration).

% --- Worker Schedule initialization
initTasksWorkersMatrix([], [], _, _, _).
initTasksWorkersMatrix( [[_TaskID,_ConstuctionID, Specialty, _BaseDuration,_Cost,_Oi,_Di,_Ei, Hi] | Operations], [NewRow | Matrix], WorkersI, Nworkers, SpecialtiesI) :-
    
    % The rows of the matrix represent the workers that will participate in a certain task (1 if the worker participates 0 otherwise)
    length(NewRow, Nworkers),
    domain(NewRow, 0, 1),
    
    % The number of workers that participate in a task is Hi
    sum(NewRow, #=, Hi),

    % At least one of the workers assigned must have the operations speciality
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
imposeNoOverLaps(Operations, WorkersMatrix) :-

    % Now each row of the matrix represents the tasks in which the worker corresponding to that line will participate
    transpose(WorkersMatrix,TransposedWorkersMatrix),
    TransposedWorkersMatrix = [SampleRow | _],
    length(SampleRow, RowLength),
    StoppingIndex is RowLength + 1,% Restriction #4
    imposeNoOverLaps_aux(Operations,TransposedWorkersMatrix,StoppingIndex).

imposeNoOverLaps_aux(_,[],_).
imposeNoOverLaps_aux(Operations,[Row|TransposedMatrix],StoppingIndex) :-

    imposeNoOverlap(1,Operations,Row,Lines,[],StoppingIndex),
    disjoint1(Lines),
    imposeNoOverLaps_aux(Operations,TransposedMatrix,StoppingIndex).

% --- Impose that a worker can't be working in to tasks at the same time
imposeNoOverlap(StoppingIndex,_,_,Lines,Lines,StoppingIndex).
imposeNoOverlap(Index,Operations,Row,Lines,LinesAcc,StoppingIndex) :-

    nth1(Index,Row,Element),
    nth1(Index,Operations,[_TaskID,_ConstuctionID, _Specialty, BaseDuration,_Cost,Oi,Di,_Ei, _Hi]),
    D #= Di * Element,
    D in 0..BaseDuration, 
    Line = line(Oi,D),
    append(LinesAcc,[Line],NewLinesAcc),
    NewIndex is Index + 1,
    imposeNoOverlap(NewIndex,Operations,Row,Lines,NewLinesAcc,StoppingIndex).

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
    getResourceCost(Tasks,ResourceCost,Acc1). 

% --- Get the cost associated with the workers' salary
getSalariesCost([],_,_,SalariesCost,SalariesCost).
getSalariesCost([task(_Oi, Di, _Ei, _Hi, _ID)|Tasks], WorkersI, [TaskWorkers|WorkersMatrix],SalariesCost,SalaryAcc) :-
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
    Duration #= End - Start,
    maximum(End, EndTimes),
    Payment #= Value + BonusFee * (ExpectedDuration - Duration).
    

getConstructionStartTimes(_, [], []).
getConstructionStartTimes(ConstructionID, [[_, ConstructionID, _, _, _, Start | _] | Operations], [Start | StartTimes]) :- !,    
    getConstructionStartTimes(ConstructionID, Operations, StartTimes).
getConstructionStartTimes(ConstructionID, [_ | Operations], StartTimes) :-
    getConstructionStartTimes(ConstructionID, Operations, StartTimes).

getConstructionEndTimes(_, [], []).
getConstructionEndTimes(ConstructionID, [[_, ConstructionID, _, _, _, _, _, End | _] | Operations], [End | EndTimes]) :- !,    
    getConstructionEndTimes(ConstructionID, Operations, EndTimes).
getConstructionEndTimes(ConstructionID, [_ | Operations], EndTimes) :-
    getConstructionEndTimes(ConstructionID, Operations, EndTimes).

%--------------------%                    
% Auxilary Functions %
%--------------------%


splitTaskVars([],[],[],[],[]).
splitTaskVars([task(Oi, Di, Ei, Hi, _) | RestTasks], [Oi|Origins],[Di|Durations],[Ei|Ends],[Hi|WorkerCounts]) :-
    splitTaskVars(RestTasks,Origins,Durations,Ends,WorkerCounts).

getVars([], []).    
getVars([task(Oi, Di, Ei, Hi, ID) | Tasks], [Oi, Di, Ei, Hi, ID| Vars]) :- getVars(Tasks, Vars). 

getPrecendenceVars([],[]). 

getPrecendenceVars([_-_#=Var|T1],[Var|T2]) :-
    getPrecendenceVars(T1,T2). 

flattenMatrix([],Result,Result).

flattenMatrix([H|T],Result,Acc) :-
    append(Acc,H,Acc1),
    flattenMatrix(T,Result,Acc1).

getTaskBaseDurations([],[]).

getTaskBaseDurations([[_TaskID, _ConstructionID, _Specialty, BaseDuration, _Cost | _] | OperationsI],[BaseDuration|Times]) :-
    getTaskBaseDurations(OperationsI,Times).

reset_timer:-statistics(total_runtime, _).

print_time(Msg):-statistics(total_runtime,[_,T]),TS is ((T//10)*10)/1000, nl,write(Msg), write(TS), write('s'), nl, nl.


% Predicates used to run the tests on the labelling flags. They don't work anymore as we changed the main solver's arguments to be simpler to test solutions.
varOrderFlags([min,max,occurrence,ff,ffc,max_regret]).
valSelectionFlags([step,enum,bisect,median]).
valOrderFlags([up,down]).

testLabelingFlags(TimeoutSeconds) :-

    varOrderFlags(OrdFlags),
    valSelectionFlags(SelFlags),
    valOrderFlags(OrdValFlags),
    findall([VarOrderFlag,ValSelFlag,ValOrderFlag],(member(VarOrderFlag,OrdFlags),member(ValSelFlag,SelFlags),member(ValOrderFlag,OrdValFlags)),LabelOptions),
    Timeout is TimeoutSeconds * 1000,
    repeatLabel(Timeout,LabelOptions),
    told.

repeatLabel(_,[]).
repeatLabel(Timeout,[CurrentOptions|LabelOptions]) :- 
    print(CurrentOptions),
    solve(_,Timeout,CurrentOptions),

    repeatLabel(Timeout,LabelOptions).

repeatLabel(Timeout,[CurrentOptions|LabelOptions]) :-
    write('FAILED\n'),
    repeatLabel(Timeout,LabelOptions).
