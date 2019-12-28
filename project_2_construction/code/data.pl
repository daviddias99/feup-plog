especialidades(['Carpintaria', 'Pichelaria', 'Canalização', 'Jardinagem', 'Telhados']).

trabalhadores([
    %[salario, [especialidades]]
    [20, [0, 0, 1, 0, 0]],
    [50, [1, 1, 0, 0, 0]],
    [60, [0, 0, 1, 0, 0]],    
    [30, [0, 0, 0, 1, 1]]
]).

operacoes([
    %[id, idObra, especialidade, tempo, custoEquipamentos]
    [1, 1, 'Pichelaria', 10, 25],
    [2, 1, 'Carpintaria', 1, 25],
    [3, 2, 'Canalização', 10, 25],
    [4, 2, 'Jardinagem', 2, 25]
]).

precedencias([
    'Carpintaria'-'Pichelaria'
]).

obras([
    %[id, preco, duracao, bonus]
    [1, 300, 10, 0],
    [2, 200, 15, 0]
]).
