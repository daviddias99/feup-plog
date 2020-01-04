:- use_module(library(clpfd)).
:- use_module(library(lists)).

operacoes([
    %[id, idObra, especialidade, tempo, custoEquipamentos]
    [1, 1, 'Carpintaria', 3, 20],
    [2, 1, 'Pichelaria', 1, 20],
    [5, 1, 'Banho', 20, 10],
    [3, 2, 'Pichelaria', 10, 20],
    [4, 2, 'Carpintaria', 2, 20]
]).

precedencias([
    'Carpintaria'-'Pichelaria',
    'Banho'-'Carpintaria'
]).

obras([
    %[id, preco, duracao, bonus]
    [1, 150, 10, 10],
    [2, 300, 15, 50]
]).


test(Result) :-
    operacoes(Ops),
    precedencias(Prec),
    obras(Obras),
    getTaskPrecedences(Obras, Prec, Ops, [], Result).

getTaskPrecedences([], _, _, Result, Result).
getTaskPrecedences([[IDobra | _] | Obras], Prec, Ops, Acc, Result) :-
    findall(Operacao, (Operacao = [_, IDobra | _], member(Operacao, Ops)), ConstructionOps),
    getConstructionTaskPrecedences(Prec, ConstructionOps, ConstructionPrec),
    append(Acc, ConstructionPrec, Acc1),
    getTaskPrecedences(Obras, Prec, Ops, Acc1, Result).

getConstructionTaskPrecedences(Precs, Ops, ConstructionPrec) :-
    findall(IDafter-IDbefore #= Dij, (member(Before-After, Precs), Op1 = [IDbefore, _, Before, _, _, Di | _], Op2 = [IDafter, _, After | _], member(Op1, Ops), member(Op2, Ops), Dij #>= Di), ConstructionPrec).


