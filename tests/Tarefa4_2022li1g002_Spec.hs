module Tarefa4_2022li1g002_Spec where

import LI12223
import Tarefa4_2022li1g002
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test testsJogoTerminou

testsJogoTerminou = [
    TestCase (assertBool "Teste para quando o jogador está num rio mas não num tronco (caiu na água)" (jogoTerminou (Jogo (Jogador (2,0)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])))),
    TestCase (assertBool "Teste para quando o jogador está num rio e num tronco" (not (jogoTerminou (Jogo (Jogador (0,0)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]))))),
    TestCase (assertBool "Teste para quando o jogador está numa estrada, nas mesmas coordenadas de um carro, ficando debaixo dele" (jogoTerminou (Jogo (Jogador (4,1)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])))),
    TestCase (assertBool "Teste para quando o jogador está numa estrada, em coordenadas onde não existe um carro" (not (jogoTerminou (Jogo (Jogador (2,1)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]))))),
    TestCase (assertBool "Teste para quando o jogador está fora do mapa, perdendo" (jogoTerminou (Jogo (Jogador (6,4)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]))))
    ]