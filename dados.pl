:- dynamic robo/2.
:- dynamic comodo/2.
:- dynamic objeto_em/2.
:- dynamic modo_fogao/1.
:- dynamic pratos_na_mesa/1.
:- dynamic estado_roupa/2.
:- dynamic estado_comida/2.

% Conexões entre cômodos (ESTÁTICAS - não dinâmicas)
comodos([sala_estar, sala_jantar, quarto, cozinha, banheiro, garagem]).

ligacao(sala_estar, sala_jantar).
ligacao(sala_estar, quarto).
ligacao(sala_estar, garagem).
ligacao(sala_estar, banheiro).
ligacao(sala_jantar, cozinha).

conectado(Comodo1, Comodo2) :- 
    (
        ligacao(Comodo1, Comodo2);
        ligacao(Comodo2, Comodo1)
    ).

% Objetos
objetos([
    tv, sofa,
    mesa, cadeira,
    cama, armario,
    fogao, geladeira,
    toalha, escova_dentes,
    caixa_ferramentas, balcao
]).

% Criar os estados iniciais do sistema
resetar_comodos :-
    retractall(comodo(_, _)),
    comodos(Lista),
    forall(
        member(Comodo, Lista),
        assertz(comodo(Comodo, sujo))
    ).

resertar_objetos :-
    retractall(objeto_em(_, _)),
    assertz(objeto_em(prato, cozinha)),
    assertz(objeto_em(roupa, quarto)),
    assertz(objeto_em(comida, cozinha)).