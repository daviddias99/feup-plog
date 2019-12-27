especialidades(['Carpintaria', 'Pichelaria', 'Canalização', 'Jardinagem', 'Telhados']).

trabalhadores([
    %[salario, [especialidades]]
    [20, [0, 0, 1, 0, 0]],
    [50, [1, 1, 0, 0, 0]],
    [60, [0, 0, 1, 0, 0]],    
    [30, [0, 0, 0, 0, 1]]
]).

operacoes([
    %[id, idObra, especialidade, tempo, custoEquipamentos]
    [1, 1, [1, 0, 0, 0, 0], 3, 20],
    [2, 1, [0, 1, 0, 0, 0], 1, 20],
    [3, 2, [0, 0, 1, 0, 0], 10, 20],
    [4, 2, [0, 0, 0, 0, 1], 2, 20]
]).

precedencias([
    'Carpintaria'-'Pichelaria'
]).

obras([
    %[id, preco, duracao, bonus]
    [1, 150, 10, 10],
    [2, 300, 15, 50]
]).
