:- dynamic robo/2.
:- dynamic comodo/2.
:- dynamic objeto_em/2.
:- dynamic modo_fogao/1.
:- dynamic pratos_na_mesa/1.
:- dynamic estado_roupa/2.
:- dynamic estado_comida/2.

% Conexões entre cômodos (ESTÁTICAS - não dinâmicas)
conectado(sala, cozinha).
conectado(sala, quarto).
conectado(sala, banheiro).
conectado(cozinha, sala).
conectado(quarto, sala).
conectado(banheiro, sala).

% ==================== INICIALIZAÇÃO CONTROLADA ====================

% Inicializa o sistema APENAS se o banco de dados estiver vazio.
% Se o robô já tiver uma posição, ele ignora e mantém o que está na memória.
inicializar_sistema :-
    robo(_, _),!, % Se já existe um robô, não faz nada (preserva o estado)
    format('~n[Aviso] Estado anterior recuperado. Use "reiniciar." para limpar tudo.~n').

inicializar_sistema :-
    \+ robo(_, _), % Só entra aqui se a memória estiver realmente limpa
    criar_estados_iniciais,
    format('~n=============================================~n'),
    format('    BEM-VINDO AO SISTEMA ROBÔ DOMÉSTICO!     ~n'),
    format('=============================================~n'),
    format('Digite "ajuda." para ver os comandos.~n'),
    format('Digite "reiniciar." para começar do zero.~n~n').

% Este predicado apenas CRIA os fatos, sem dar retractall.
% O retractall fica reservado para o comando manual "reiniciar".
criar_estados_iniciais :-
    assertz(robo(sala, nada)),
    assertz(comodo(sala, sujo)),
    assertz(comodo(cozinha, limpo)),
    assertz(comodo(quarto, sujo)),
    assertz(comodo(banheiro, sujo)),
    assertz(modo_fogao(desligado)),
    assertz(pratos_na_mesa([])),
    
    % Objetos em cada cômodo
    assertz(objeto_em(almofada, sala)),
    assertz(objeto_em(abajur, sala)),
    assertz(objeto_em(cesto, sala)),
    assertz(objeto_em(vaso_decorativo, sala)),
    assertz(objeto_em(livro, sala)),
    assertz(objeto_em(controle, sala)),
    
    assertz(objeto_em(tabua, cozinha)),
    assertz(objeto_em(frigideira, cozinha)),
    assertz(objeto_em(carne_crua, cozinha)),
    assertz(objeto_em(macarrao_cru, cozinha)),
    assertz(objeto_em(feijao_cru, cozinha)),
    assertz(objeto_em(copo, cozinha)),
    assertz(objeto_em(panela, cozinha)),
    assertz(objeto_em(prato_vazio, cozinha)),
    assertz(objeto_em(geladeira, cozinha)),
    
    assertz(objeto_em(travesseiro, quarto)),
    assertz(objeto_em(cobertor, quarto)),
    assertz(objeto_em(caixa_jóias, quarto)),
    assertz(objeto_em(perfume, quarto)),
    assertz(objeto_em(ferro, quarto)),
    assertz(objeto_em(cama, quarto)),
    assertz(objeto_em(armario, quarto)),
    
    assertz(objeto_em(toalha_banho, banheiro)),
    assertz(objeto_em(toalha_rosto, banheiro)),
    assertz(objeto_em(escova_dente, banheiro)),
    assertz(objeto_em(kit_limpeza, banheiro)),
    assertz(objeto_em(saboneteira, banheiro)),
    assertz(objeto_em(papel_guardanapo, banheiro)),
    
    % Estados iniciais das roupas
    assertz(estado_roupa(camisa, amarrotada)),
    assertz(estado_roupa(calca, amarrotada)),
    assertz(estado_roupa(camiseta, amarrotada)),
    
    % Estados iniciais das comidas
    assertz(estado_comida(carne, cru)),
    assertz(estado_comida(feijao, cru)),
    assertz(estado_comida(macarrao, cru)).

% Inicialização automática ao carregar o arquivo
:- initialization(inicializar_sistema).

% ==================== FUNÇÕES AUXILIARES ====================

% Verifica se está segurando algo
segurando_algo :-
    robo(_, Objeto),
    Objeto \= nada.

% Obtém o objeto que o robô está segurando
segurando_atual(Objeto) :-
    robo(_, Objeto),
    Objeto \= nada.

% Libera as mãos se estiver segurando algo
liberar_maos :-
    segurando_algo,
    !,
    soltar.
liberar_maos.

% Obtém a localização atual do robô
local_atual(Local) :-
    robo(Local, _).

% Verifica estado do cômodo
estado_comodo(Local, Estado) :-
    comodo(Local, Estado).

% Verifica se há espaço na mesa
mesa_cheia :-
    pratos_na_mesa(Lista),
    length(Lista, Numero),
    Numero >= 3.

% Verifica se pode cozinhar
pode_cozinhar :-
    \+ mesa_cheia.

% ==================== NOVAS REGRAS ADICIONADAS ====================

% 1. Cozinhar todos os pratos
cozinhar_tudo :-
    format('~n=== INICIANDO COZINHA DE TODOS OS PRATOS ===~n~n'),
    cozinhar_uma_vez(carne),
    cozinhar_uma_vez(feijao),
    cozinhar_uma_vez(macarrao),
    format('~n=== TODOS OS PRATOS COZINHADOS! ===~n').

% Auxiliar para cozinhar uma comida, verificando se já está cozida
cozinhar_uma_vez(Comida) :-
    estado_comida(Comida, Estado),
    (Estado = cozido ->
        format('~w já está cozido. Pulando...~n', [Comida])
    ;
        cozinhar(Comida)
    ).

% 2. Limpar todos os cômodos
limpar_tudo :-
    format('~n=== INICIANDO LIMPEZA DE TODOS OS CÔMODOS ===~n~n'),
    limpar_se_sujo(sala),
    limpar_se_sujo(cozinha),
    limpar_se_sujo(quarto),
    limpar_se_sujo(banheiro),
    format('~n=== TODOS OS CÔMODOS LIMPOS! ===~n').

% Auxiliar para limpar apenas se estiver sujo
limpar_se_sujo(Comodo) :-
    estado_comodo(Comodo, Estado),
    (Estado = limpo ->
        format('~w já está limpo. Pulando...~n', [Comodo])
    ;
        limpar(Comodo)
    ).

% 3. Passar todas as roupas (já existe no comando passar)
% O comando passar já passa todas as roupas amarrotadas

% 4. Fazer tudo: passar roupas, cozinhar e limpar
fazer_tudo :-
    format('~n=============================================~n'),
    format('          EXECUTANDO TAREFAS COMPLETAS       ~n'),
    format('=============================================~n~n'),
    
    % Mostrar estado inicial
    format('=== ESTADO INICIAL ===~n'),
    estado_geral_atual,
    
    % Passar todas as roupas
    format('~n=== PASSSANDO TODAS AS ROUPAS ===~n'),
    passar_todas_roupas,
    
    % Cozinhar todos os pratos
    format('~n=== COZINHANDO TODOS OS PRATOS ===~n'),
    cozinhar_tudo,
    
    % Limpar todos os cômodos (cozinhar suja a cozinha)
    format('~n=== LIMPANDO TODOS OS CÔMODOS ===~n'),
    limpar_tudo,
    
    % Mostrar estado final
    format('~n=== ESTADO FINAL ===~n'),
    estado_geral_atual,
    
    format('~n=== TODAS AS TAREFAS CONCLUÍDAS! ===~n').

% Auxiliar para passar todas as roupas
passar_todas_roupas :-
    findall(Roupa, estado_roupa(Roupa, amarrotada), RoupasAmarrotadas),
    (RoupasAmarrotadas = [] ->
        format('Não há roupas amarrotadas para passar.~n')
    ;
        length(RoupasAmarrotadas, Num),
        format('Encontradas ~w roupas amarrotadas para passar.~n', [Num]),
        forall(member(Roupa, RoupasAmarrotadas), passar_roupa(Roupa))
    ).

% Auxiliar para passar uma roupa específica
passar_roupa(Roupa) :-
    format('Passando ~w...~n', [Roupa]),
    % Simular o processo de passar
    retract(estado_roupa(Roupa, amarrotada)),
    assertz(estado_roupa(Roupa, passada)),
    format('~w passada com sucesso!~n', [Roupa]).

% ==================== REGRAS DE MOVIMENTO ====================

% Mostra trajetória completa entre dois cômodos
mostrar_caminho(Inicio, Fim, Caminho) :-
    encontrar_caminho(Inicio, Fim, [Inicio], CaminhoInverso),
    reverse(CaminhoInverso, Caminho).

encontrar_caminho(Atual, Fim, Visitados, [Fim|Visitados]) :-
    (conectado(Atual, Fim); conectado(Fim, Atual)).

encontrar_caminho(Atual, Fim, Visitados, Caminho) :-
    (conectado(Atual, Proximo); conectado(Proximo, Atual)),
    Proximo \= Fim,
    \+ member(Proximo, Visitados),
    encontrar_caminho(Proximo, Fim, [Proximo|Visitados], Caminho).

% Movimenta o robô automaticamente para um destino
ir_para(Destino) :-
    local_atual(Atual),
    (Atual == Destino ->
        format('Robô já está em ~w.~n', [Destino])
    ;
        mostrar_caminho(Atual, Destino, Caminho),
        percorrer_caminho(Caminho)
    ).

percorrer_caminho([_]) :- !.
percorrer_caminho([Atual, Proximo|Resto]) :-
    andar_detalhado(Atual, Proximo),
    percorrer_caminho([Proximo|Resto]).

% Andar com detalhamento
andar_detalhado(De, Para) :-
    retract(robo(De, Segurando)),
    assertz(robo(Para, Segurando)),
    format('Robô se moveu de ~w para ~w', [De, Para]),
    (Segurando \= nada ->
        format(' (segurando ~w)', [Segurando])
    ;
        true
    ),
    format('.~n').

% Comando andar para usuário
andar(Cômodo) :-
    local_atual(Atual),
    (conectado(Atual, Cômodo) ; conectado(Cômodo, Atual)),
    andar_detalhado(Atual, Cômodo).

andar(Cômodo) :-
    format('Não é possível ir diretamente para ~w. Use ir_para(~w).~n', [Cômodo, Cômodo]),
    fail.

% ==================== MANIPULAÇÃO DE OBJETOS ====================

pegar(Objeto) :-
    local_atual(Local),
    objeto_em(Objeto, Local),
    \+ segurando_algo,
    retract(objeto_em(Objeto, Local)),
    retract(robo(Local, _)),
    assertz(robo(Local, Objeto)),
    format('Robô pegou ~w em ~w.~n', [Objeto, Local]).

pegar(_) :-  % Correção: variável anônima
    segurando_algo,
    format('Robô já está segurando algo. Solte primeiro.~n'),
    fail.

pegar(Objeto) :-
    format('~w não está aqui ou não existe.~n', [Objeto]),
    fail.

soltar :-
    segurando_algo,
    robo(Local, Objeto),
    retract(robo(Local, Objeto)),
    assertz(robo(Local, nada)),
    assertz(objeto_em(Objeto, Local)),
    format('Robô soltou ~w em ~w.~n', [Objeto, Local]).

soltar :-
    \+ segurando_algo,
    format('Robô não está segurando nada.~n'),
    fail.

% ==================== TAREFA DE LIMPEZA (CORRIGIDA) ====================

limpar(Local) :-
    % Verificar se já está limpo
    estado_comodo(Local, limpo),
    !,
    format('~w já está limpo.~n', [Local]).

limpar(Local) :-
    % Verificar se está sujo
    estado_comodo(Local, sujo),
    !,  % Cut para commitar a esta cláusula
    % Iniciar processo de limpeza
    format('Iniciando limpeza de ~w...~n', [Local]),
    
    % Fase 1: Preparação - ir ao banheiro pegar kit
    preparar_para_limpar(Local),
    
    % Fase 2: Executar limpeza
    executar_limpeza(Local),
    
    % Fase 3: Finalização - voltar e guardar kit
    finalizar_limpeza,
    
    format('Limpeza de ~w concluída com sucesso!~n', [Local]),
    !.  % Cut final para evitar backtracking

% Fase 1: Preparação
preparar_para_limpar(_) :-
    % Liberar mãos se segurando algo
    liberar_maos,
    
    % Ir para o banheiro se necessário
    local_atual(Atual),
    (Atual \= banheiro ->
        format('Indo para o banheiro pegar kit de limpeza...~n'),
        ir_para(banheiro)
    ; true),
    
    % Pegar kit de limpeza
    (objeto_em(kit_limpeza, banheiro) ->
        pegar(kit_limpeza),
        format('Kit de limpeza adquirido.~n')
    ;
        format('ERRO: Kit de limpeza não encontrado no banheiro!~n'),
        fail
    ).

% Fase 2: Execução
executar_limpeza(Local) :-
    % Ir para o local a ser limpo
    local_atual(Atual),
    (Atual \= Local ->
        format('Indo para ~w para limpar...~n', [Local]),
        ir_para(Local)
    ; true),
    
    % Executar limpeza
    retract(comodo(Local, sujo)),
    assertz(comodo(Local, limpo)),
    format('Robô limpou ~w.~n', [Local]).

% Fase 3: Finalização
finalizar_limpeza :-
    % Verificar se ainda está segurando o kit
    (segurando_atual(kit_limpeza) ->
        % Voltar ao banheiro se necessário
        local_atual(Atual),
        (Atual \= banheiro ->
            format('Retornando ao banheiro para guardar kit...~n'),
            ir_para(banheiro)
        ; true),
        
        % Guardar kit
        soltar,
        format('Kit de limpeza guardado.~n')
    ;
        true  % Já foi guardado
    ).

% ==================== TAREFA DE PASSAR ====================

passar :-
    % Verificar se há roupa para passar
    \+ estado_roupa(_, amarrotada),
    !,
    format('Não há roupas amarrotadas para passar.~n').

passar :-
    % Liberar mãos
    liberar_maos,
    
    % Ir para o quarto
    local_atual(Atual),
    (Atual \= quarto ->
        format('Indo para o quarto para passar roupa...~n'),
        ir_para(quarto)
    ; true),
    
    % Encontrar roupa amarrotada
    estado_roupa(Roupa, amarrotada),
    format('Encontrada ~w amarrotada.~n', [Roupa]),
    
    % Simular processo de passar
    retract(estado_roupa(Roupa, amarrotada)),
    assertz(estado_roupa(Roupa, passada)),
    
    % Pegar e usar ferro
    (objeto_em(ferro, quarto) ->
        pegar(ferro),
        format('Passando ~w com o ferro...~n', [Roupa]),
        soltar
    ; true),
    
    format('~w passada e guardada.~n', [Roupa]).

% ==================== TAREFA DE COZINHAR (CORRIGIDA) ====================

cozinhar(Comida) :-
    % Verificar comida válida
    \+ member(Comida, [carne, feijao, macarrao]),
    !,
    format('~w não é uma comida válida.~n', [Comida]),
    fail.

cozinhar(Comida) :-
    % Verificar se já está cozido
    estado_comida(Comida, cozido),
    !,
    format('~w já está cozido.~n', [Comida]).

cozinhar(_) :-  % Correção: variável anônima
    % Verificar se pode cozinhar
    \+ pode_cozinhar,
    !,
    format('Mesa cheia! Não é possível cozinhar mais.~n'),
    fail.

cozinhar(Comida) :-
    % Iniciar processo de cozinhar
    format('Iniciando preparo de ~w...~n', [Comida]),
    
    % Liberar mãos
    liberar_maos,
    
    % Ir para a cozinha
    local_atual(Atual),
    (Atual \= cozinha ->
        format('Indo para a cozinha...~n'),
        ir_para(cozinha)
    ; true),
    
    % Ligar fogão
    retract(modo_fogao(desligado)),
    assertz(modo_fogao(ligado)),
    format('Fogão ligado.~n'),
    
    % Cozinhar
    format('Cozinhando ~w...~n', [Comida]),
    retract(estado_comida(Comida, cru)),
    assertz(estado_comida(Comida, cozido)),
    
    % Desligar fogão
    retract(modo_fogao(ligado)),
    assertz(modo_fogao(desligado)),
    format('Fogão desligado.~n'),
    
    % Servir na mesa
    atom_concat(Comida, '_cozido', PratoCozido),
    retract(pratos_na_mesa(ListaAtual)),
    append(ListaAtual, [PratoCozido], NovaLista),
    assertz(pratos_na_mesa(NovaLista)),
    
    % Atualizar estado da cozinha - CORREÇÃO: usar retractall para qualquer estado
    retractall(comodo(cozinha, _)),
    assertz(comodo(cozinha, sujo)),
    
    % Mostrar resultado
    length(NovaLista, N),
    format('~w pronto! (~w/3 pratos na mesa)~n', [Comida, N]).

% ==================== COMANDOS DE INFORMAÇÃO ====================

estado_geral_atual :-
    format('~n========== ESTADO GERAL ATUAL ==========~n'),
    
    % Robô
    local_atual(Local),
    format('Robô está em: ~w~n', [Local]),
    (segurando_atual(Objeto) -> 
        format('Segurando: ~w~n', [Objeto])
    ;
        format('Segurando: nada~n')
    ),
    
    % Cômodos - corrigido sem call dinâmico
    format('~n--- Estado dos Cômodos ---~n'),
    comodo(sala, EstadoSala), format('Sala: ~w~n', [EstadoSala]),
    comodo(cozinha, EstadoCozinha), format('Cozinha: ~w~n', [EstadoCozinha]),
    comodo(quarto, EstadoQuarto), format('Quarto: ~w~n', [EstadoQuarto]),
    comodo(banheiro, EstadoBanheiro), format('Banheiro: ~w~n', [EstadoBanheiro]),
    
    % Fogão
    format('~n--- Fogão ---~n'),
    modo_fogao(EstadoFogao),
    format('Estado: ~w~n', [EstadoFogao]),
    
    % Mesa
    format('~n--- Mesa ---~n'),
    pratos_na_mesa(Lista),
    (Lista = [] -> 
        format('Vazia (0/3)~n')
    ;
        length(Lista, N),
        (N >= 3 -> 
            format('Cheia (~w/3): ~w~n', [N, Lista])
        ;
            format('~w/~w: ~w~n', [N, 3, Lista])
        )
    ),
    
    % Roupas
    format('~n--- Estados das Roupas ---~n'),
    (estado_roupa(_, _) ->
        findall(R-E, estado_roupa(R, E), Roupas),
        forall(member(R-E, Roupas), format('~w: ~w~n', [R, E]))
    ;
        format('Sem informações de roupas.~n')
    ),
    
    % Comidas
    format('~n--- Estados das Comidas ---~n'),
    (estado_comida(_, _) ->
        findall(C-E, estado_comida(C, E), Comidas),
        forall(member(C-E, Comidas), format('~w: ~w~n', [C, E]))
    ;
        format('Sem informações de comidas.~n')
    ),
    
    format('~n=====================================~n~n').

onde_estou :-
    local_atual(Local),
    (segurando_atual(Objeto) ->
        format('Robô está em ~w segurando ~w.~n', [Local, Objeto])
    ;
        format('Robô está em ~w com as mãos livres.~n', [Local])
    ).

o_que_tenho :-
    (segurando_atual(Objeto) ->
        format('Robô está segurando ~w.~n', [Objeto])
    ;
        format('Robô não está segurando nada.~n')
    ).

estado_comodos :-
    format('Estado dos cômodos:~n'),
    comodo(sala, EstadoSala), format('  Sala: ~w~n', [EstadoSala]),
    comodo(cozinha, EstadoCozinha), format('  Cozinha: ~w~n', [EstadoCozinha]),
    comodo(quarto, EstadoQuarto), format('  Quarto: ~w~n', [EstadoQuarto]),
    comodo(banheiro, EstadoBanheiro), format('  Banheiro: ~w~n', [EstadoBanheiro]).

pratos_na_mesa :-
    pratos_na_mesa(Lista),
    (Lista = [] ->
        format('Não há pratos na mesa.~n')
    ;
        length(Lista, N),
        format('Pratos na mesa (~w/3): ~w~n', [N, Lista])
    ).

estado_fogao :-
    modo_fogao(Estado),
    format('Fogão está ~w.~n', [Estado]).

% ==================== REINICIAR AMBIENTE (MANUAL) ====================

% Este é o único lugar que deve limpar a memória obrigatoriamente.
reiniciar :-
    retractall(robo(_, _)),
    retractall(comodo(_, _)),
    retractall(objeto_em(_, _)),
    retractall(modo_fogao(_)),
    retractall(pratos_na_mesa(_)),
    retractall(estado_roupa(_, _)),
    retractall(estado_comida(_, _)),
    criar_estados_iniciais,
    format('~n=============================================~n'),
    format('    AMBIENTE REINICIADO DO ZERO!             ~n'),
    format('=============================================~n~n').

% ==================== AJUDA (ATUALIZADA) ====================

ajuda :-
    format('~n=============================================~n'),
    format('          SISTEMA ROBÔ DOMÉSTICO           ~n'),
    format('=============================================~n'),
    format('~nCOMANDOS PRINCIPAIS:~n'),
    format('  ir_para(cozinha).    - Vai para cozinha (mostra trajetória)~n'),
    format('  andar(cozinha).      - Move para cozinha (apenas se conectado)~n'),
    format('  pegar(objeto).       - Pega um objeto~n'),
    format('  soltar.              - Solta o objeto atual~n'),
    format('~nTAREFAS AUTOMÁTICAS:~n'),
    format('  limpar(sala).        - Limpa um cômodo~n'),
    format('  cozinhar(carne).     - Cozinha carne~n'),
    format('  cozinhar(feijao).    - Cozinha feijão~n'),
    format('  cozinhar(macarrao).  - Cozinha macarrão~n'),
    format('  passar.              - Passa roupa no quarto~n'),
    format('~nTAREFAS EM LOTE (NOVOS):~n'),
    format('  cozinhar_tudo.       - Cozinha todos os pratos~n'),
    format('  limpar_tudo.         - Limpa todos os cômodos~n'),
    format('  fazer_tudo.          - Executa todas as tarefas (passar, cozinhar, limpar)~n'),
    format('~nINFORMAÇÕES:~n'),
    format('  estado_geral_atual.  - Mostra situação completa~n'),
    format('  onde_estou.          - Mostra local e o que segura~n'),
    format('  o_que_tenho.         - Mostra o que está segurando~n'),
    format('  estado_comodos.      - Mostra estado dos cômodos~n'),
    format('  pratos_na_mesa.      - Mostra pratos na mesa~n'),
    format('  estado_fogao.        - Mostra estado do fogão~n'),
    format('~nSISTEMA:~n'),
    format('  reiniciar.           - Reinicia todo o ambiente~n'),
    format('  ajuda.               - Mostra esta mensagem~n'),
    format('~n=============================================~n').