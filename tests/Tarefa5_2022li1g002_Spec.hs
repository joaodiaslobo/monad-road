module Tarefa5_2022li1g002_Spec where

import LI12223
import Tarefa5_2022li1g002
import Test.HUnit

jogoTeste01 = Jogo (Jogador (2,2)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
jogoTeste02 = Jogo (Jogador (2,1)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
jogoTeste03 = Jogo (Jogador (0,0)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
jogoTeste04 = Jogo (Jogador (1,1)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
jogoTeste05 = Jogo (Jogador (2,0)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])


testsT5 :: Test
testsT5 = TestLabel "Testes Tarefa 5" $ test testsDeslizaJogo

testsDeslizaJogo = [
    "Deslize de um mapa de teste com seed 32" ~: Jogo (Jogador (2,3)) (Mapa 5 [(Rio (-1),[Tronco,Tronco,Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum,Nenhum,Carro,Carro])]) ~=? deslizaJogo 32 jogoTeste01,
    "Deslize de um mapa de teste com seed 83" ~: Jogo (Jogador (2,2)) (Mapa 5 [(Estrada (-1),[Carro,Carro,Carro,Nenhum,Nenhum]),(Rio 3,[Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum,Nenhum,Carro,Carro])]) ~=? deslizaJogo 83 jogoTeste02,
    "Deslize de um mapa de teste com seed 23" ~: Jogo (Jogador (0,1)) (Mapa 5 [(Relva,[Nenhum,Nenhum,Arvore,Arvore,Arvore]),(Rio 3,[Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum,Nenhum,Carro,Carro])]) ~=? deslizaJogo 23 jogoTeste03,
    "Deslize de um mapa de teste com seed 43" ~: Jogo (Jogador (1,2)) (Mapa 5 [(Estrada (-2),[Carro,Nenhum,Carro,Carro,Nenhum]),(Rio 3,[Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum,Nenhum,Carro,Carro])]) ~=? deslizaJogo 43 jogoTeste04,
    "Deslize de um mapa de teste com seed 99" ~: Jogo (Jogador (2,1)) (Mapa 5 [(Estrada (-2),[Nenhum,Carro,Nenhum,Nenhum,Nenhum]),(Rio 3,[Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum,Nenhum,Carro,Carro])]) ~=? deslizaJogo 99 jogoTeste05
    ]