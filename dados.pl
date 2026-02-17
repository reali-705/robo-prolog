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