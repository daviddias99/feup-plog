:- use_module(library(clpfd)).
:- use_module(library(lists)).

especialidades([carpinteiro, picheleiro, canalizador, telhados, jardineiro]).

trabalhadores([
    %[salario, [especialidades]]
    [20, [0, 0, 1, 0, 0]],
    [50, [1, 1, 0, 0, 0]],
    [60, [0, 0, 1, 0, 0]],    
    [30, [0, 0, 0, 0, 1]]
]).

operacoes([
    %[id, especialidade, tempo, custoEquipamentos, ]
    [1, [1, 0, 0, 0, 0], 3, 20],
    [2, [0, 1, 0, 0, 0], 1, 20],
    [3, [0, 0, 1, 0, 0], 10, 20],
    [4, [0, 0, 0, 0, 1], 2, 20]
]).

precedencias([
    canalizador-jardineiro
]).

    %preco, duracao, bonus
obra([150, 10, 10]).


dostuff(Vars) :-
    tell('file.txt'),
    % Fetch variables
    trabalhadores(T),
    operacoes(Ops),
    precedencias(Prec),
    obra([Custo, Duracao, Bonus]),

    % Init tasks
    length(T, Nworkers),
    initTasks(Ops, Tasks, Nworkers),

    % Restriction #1
    cumulative(Tasks, [limit(Nworkers)]),
    
    % Restriction #2, #3 and #4
    initTasksWorkersMatrix(Ops, Tasks, Matrix, T, Nworkers),

    % Resticton #5
    transpose(Matrix,TransposedMatrix),
    imposeNoOverLaps(Tasks,TransposedMatrix),

    % Solution search
    getVars(Tasks, Vars),
    flattenMatrix(Matrix,FlatMatrix,[]),
    append(Vars,FlatMatrix,FinalVars),
    labeling([], FinalVars),
    write(Tasks), write('\n'),write(Matrix),
    told.

initTasks([], [], _).
initTasks([[ID, _Esp, Dbase | _] | RestOps], [task(Oi, Di, Ei, Hi, ID) | RestTasks], Nworkers) :-
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