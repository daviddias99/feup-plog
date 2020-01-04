especialidades([
    %nomeEspecialidade
]).

trabalhadores([
    %[salario, [especialidades]]

]).

operacoes([
    %[id, idObra, especialidade, tempo, custoEquipamentos]
]).

precedencias([
    %nomeEspecialidadeA-nomeEspecialidadeB
]).

obras([
    %[id, preco, duracao, bonus]
]).
