:- use_module(library(clpfd)).
:- use_module(library(lists)).

especialidades([carpinteiro, picheleiro, canalizador, telhados, jardineiro]).

trabalhadores([
    %[salario, [especialidades]]
    [20, []],
    [50, [carpinteiro, picheleiro]],
    [60, [canalizador]],    
    [30, [jardineiro]]
]).

operacoes([
    %[id, especialidade, tempo, custoEquipamentos, ]
    [1, carpinteiro, 3, 20],
    [2, picheleiro, 1, 20],
    [3, canalizador, 10, 20],
    [4, jardineiro, 2, 20]
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
    initTasks(Ops, Tasks),
    print(Tasks),
    initWorkers(T, Capacities),
    print(Capacities),
    multi_cumulative(Tasks, Capacities, []),
    getVars(Tasks, Vars),
    labeling([], Vars).

initTasks([], []).
initTasks([[ID, _Esp, Di | _] | RestOps], [task(Oi, Di, Ei, Hsi, ID) | RestTasks]) :-
    %length(Hsi, Length), NewDi #= Di + Length,  
    length(Hsi, 4),
    domain(Hsi, 0, 1),
    domain([Oi, Ei], 0, 50),
    initTasks(RestOps, RestTasks).

initWorkers([], []).
initWorkers([_ | RestWorkers], [cumulative(1) | RestCumulative]) :- initWorkers(RestWorkers, RestCumulative). 

getVars([task(Oi, Di, Ei, Hsi, _) | Tasks], [Oi, Di, Ei | Vars]) :- getVars(Tasks, SubVars), append(Hsi, SubVars, Vars). 
