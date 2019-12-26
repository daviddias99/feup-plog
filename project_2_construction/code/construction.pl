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
    trabalhadores(T),
    operacoes(Ops),
    precedencias(Prec),
    obra([Custo, Duracao, Bonus]),
    length(T, Nworkers),
    initTasks(Ops, Tasks, Nworkers),
    print(Tasks),
    cumulative(Tasks, [limit(Nworkers)]),
    initTasksWorkersMatrix(Ops, Tasks, Matrix, W, Nworkers),
    getVars(Tasks, Vars),
    labeling([], Vars).

initTasks([], [], _).
initTasks([[ID, _Esp, Dbase | _] | RestOps], [task(Oi, Di, Ei, Hi, ID) | RestTasks], Nworkers) :-
    domain([Ei, Oi], 0, 100),
    Di in 1..Dbase,
    Hi in 1..Nworkers,
    Di #= Dbase / Hi,
    initTasks(RestOps, RestTasks, Nworkers).

getVars([], []).    
getVars([task(Oi, Di, Ei, Hi, _) | Tasks], [Oi, Di, Ei, Hi | Vars]) :- getVars(Tasks, Vars). 


initTasksWorkersMatrix([], [], [], _).
initTasksWorkersMatrix([[_ID, Esp, _Dbase, _Custo] | Ops], [task(_Oi, _Di, _Ei, Hi, _ID) | Tasks], [NewRow | Matrix], Workers, Nworkers) :-
    length(NewRow, Nworkers),
    domain(NewRow, 0, 1),
    sum(NewRow, #=, Hi),
    atLeastOneSpecialty(1, NewRow, Esp, Workers),
    initTasksWorkersMatrix(Ops, Tasks, Workers, Nworkers).

atLeastOneSpecialty(I, NewRow, Specialty, Workers) :-
    specialtyList(Specialty, Workers, SpecialtyList),
    scalar_product(SpecialtyList, NewRow, #>, 0).

specialtyList(_, [], []).
specialtyList(Specialty, [[_, WorkerSpecialties] | Workers], [B | List]) :-
    scalar_product_reif(WorkerSpecialties, Specialty, #>, 0, B),
    specialtyList(Specialty, Workers, List).



