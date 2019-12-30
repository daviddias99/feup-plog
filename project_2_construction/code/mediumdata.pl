especialidades(['Carpintaria', 'Pichelaria', 'Canalização', 'Jardinagem', 'Telhados', 'Chaminés', 'Eletricidade']).

trabalhadores([
    %[salario, [especialidades]]
    [20, [0, 0, 0, 0, 0, 0, 0]],
    [50, [1, 1, 0, 0, 0, 0, 0]],
    [60, [0, 0, 1, 0, 0, 0, 0]],    
    [70, [0, 0, 0, 1, 1, 0, 0]],
    [80, [0, 0, 1, 0, 0, 1, 0]],
    [50, [1, 1, 0, 0, 0, 0, 0]],
    [60, [0, 0, 1, 0, 0, 0, 1]]    
]).

operacoes([
    %[id, idObra, especialidade, tempo, custoEquipamentos]
    [1, 1, 'Pichelaria', 10, 25],
    [2, 1, 'Carpintaria', 1, 50],
    [3, 1, 'Jardinagem', 2, 20],
    [4, 1, 'Eletricidade', 5, 100],
    [5, 1, 'Canalização', 4, 120],
    [6, 2, 'Pichelaria', 3, 25],
    [7, 2, 'Carpintaria', 5, 50],
    [8, 2, 'Jardinagem', 2, 20],
    [9, 2, 'Eletricidade', 6, 100]
]).

precedencias([
    'Carpintaria'-'Pichelaria',
    'Telhado'-'Chaminés',
    'Canalização'-'Jardinagem',
    'Eletricidade'-'Carpintaria'
]).

obras([
    %[id, preco, duracao, bonus]
    [1, 1000, 10, 10],
    [2, 800, 15, 20]

]).
