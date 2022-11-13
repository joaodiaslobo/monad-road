module Tarefa3_2022li1g002_Spec where

import LI12223
import Tarefa3_2022li1g002
import Test.HUnit

jogoTeste01 = Jogo (Jogador (2,2)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
jogoTeste02 = Jogo (Jogador (2,1)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
jogoTeste03 = Jogo (Jogador (0,0)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
jogoTeste04 = Jogo (Jogador (1,1)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
jogoTeste05 = Jogo (Jogador (2,0)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

jogadaTeste01 = Move Direita
jogadaTeste02 = Move Cima
jogadaTeste03 = Move Baixo
jogadaTeste04 = Move Esquerda
jogadaTeste05 = Parado

jogadorTeste01 = Jogador (2,2)
jogadorTeste02 = Jogador (2,1)
jogadorTeste03 = Jogador (0,0)
jogadorTeste04 = Jogador (1,1)
jogadorTeste05 = Jogador (2,0)

linhasTeste01 = [(Rio 3, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Estrada (-2), [Nenhum, Nenhum, Nenhum, Carro, Nenhum]),(Relva, [Nenhum, Arvore, Arvore, Nenhum, Arvore])]
linhasTeste02 = [(Rio 2, [Tronco, Nenhum, Nenhum, Nenhum, Tronco]),(Estrada (-1), [Nenhum, Carro, Carro, Nenhum, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore])]
linhasTeste03 = [(Rio 1, [Nenhum, Nenhum, Tronco, Nenhum, Nenhum]),(Estrada 1, [Carro, Nenhum, Nenhum, Carro, Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Arvore, Nenhum])]
linhasTeste04 = [(Rio (-1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Estrada 2, [Nenhum, Carro, Nenhum, Carro, Carro]),(Relva, [Nenhum, Nenhum, Nenhum, Arvore, Arvore])]
linhasTeste05 = [(Rio (-2), [Tronco, Tronco, Tronco, Nenhum, Nenhum]),(Estrada 3, [Nenhum, Nenhum, Carro, Carro, Nenhum]),(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore])]

obstaculoTeste01 = [Tronco, Nenhum, Tronco, Nenhum, Tronco]
obstaculoTeste02 = [Nenhum, Tronco, Nenhum, Tronco, Nenhum]
obstaculoTeste03 = [Carro, Nenhum, Nenhum, Carro, Nenhum]
obstaculoTeste04 = [Nenhum, Carro, Carro, Nenhum, Carro]
obstaculoTeste05 = [Nenhum, Carro, Nenhum, Carro, Nenhum]

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test (testsAnimaJogo++testsMoveJogador++testsAnimaObstaculos++testsAnimaObstaculoAux)

testsAnimaJogo = [
    "Teste para quando o jogador não se move para a direita à conta de uma árvore" ~: Jogo (Jogador (2,2)) (Mapa 5 [(Rio 3,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore])]) ~=? animaJogo jogoTeste01 jogadaTeste01,
    "Teste para quando o jogador se move para cima, duma estrada para um rio, tentando ficar num tronco" ~: Jogo (Jogador (2,0)) (Mapa 5 [(Rio 3,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore])]) ~=? animaJogo jogoTeste02 jogadaTeste02,
    "Teste para quando o jogador se move para baixo, dum rio para uma estrada" ~: Jogo (Jogador (0,1)) (Mapa 5 [(Rio 3,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore])]) ~=? animaJogo jogoTeste03 jogadaTeste03,
    "Teste para quando o jogador se move para a esquerda, tentando fugir de um carro numa estrada" ~: Jogo (Jogador (0,1)) (Mapa 5 [(Rio 3,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore])]) ~=? animaJogo jogoTeste04 jogadaTeste04,
    "Teste para quando o jogador fica parado num tronco, movendo-se apenas com a velocidade do rio" ~: Jogo (Jogador (5,0)) (Mapa 5 [(Rio 3,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore])]) ~=? animaJogo jogoTeste05 jogadaTeste05
    ] 

testsMoveJogador = [
    "O jogador move-se para a direita" ~: Jogador (3,2) ~=? moveJogador jogadorTeste01 jogadaTeste01 linhasTeste01,
    "O jogador move-se para cima" ~: Jogador (2,0) ~=? moveJogador jogadorTeste02 jogadaTeste02 linhasTeste02,
    "O jogador move-se para baixo" ~: Jogador (0,1) ~=? moveJogador jogadorTeste03 jogadaTeste03 linhasTeste03,
    "O jogador move-se para a esquerda" ~: Jogador (0,1) ~=? moveJogador jogadorTeste04 jogadaTeste04 linhasTeste04,
    "O jogador fica parado" ~: Jogador (0,0) ~=? moveJogador jogadorTeste05 jogadaTeste05 linhasTeste05
    ]

testsAnimaObstaculos = [
    "Teste que anima obstáculos do exemplo 1" ~: [(Rio 3,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Estrada (-2),[Nenhum,Carro,Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore])] ~=? animaObstaculos linhasTeste01,
    "Teste que anima obstáculos do exemplo 2" ~: [(Rio 2,[Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore])] ~=? animaObstaculos linhasTeste02,
    "Teste que anima obstáculos do exemplo 3" ~: [(Rio 1,[Nenhum,Nenhum,Nenhum,Tronco,Nenhum]),(Estrada 1,[Nenhum,Carro,Nenhum,Nenhum,Carro]),(Relva,[Arvore,Arvore,Nenhum,Arvore,Nenhum])] ~=? animaObstaculos linhasTeste03,
    "Teste que anima obstáculos do exemplo 4" ~: [(Rio (-1),[Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Estrada 2,[Carro,Carro,Nenhum,Carro,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Arvore])] ~=? animaObstaculos linhasTeste04,
    "Teste que anima obstáculos do exemplo 5" ~: [(Rio (-2),[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore])] ~=? animaObstaculos linhasTeste05
    ]

testsAnimaObstaculoAux = [
    "Teste para quando os obstáculos pertencem a um rio e a velocidade é positiva" ~: [Tronco,Nenhum,Tronco,Tronco,Nenhum] ~=? animaObstaculoAux 3 obstaculoTeste01 0 ,
    "Teste para quando os obstáculos pertencem a um rio e a velocidade é negativa" ~: [Tronco,Nenhum,Tronco,Nenhum,Nenhum] ~=? animaObstaculoAux (-1) obstaculoTeste02 0 ,
    "Teste para quando os obstáculos pertencem a uma estrada e a velocidade é negativa" ~: [Carro,Nenhum,Carro,Nenhum,Nenhum] ~=? animaObstaculoAux (-3) obstaculoTeste03 0 ,
    "Teste para quando os obstáculos pertencem a uma estrada e a velocidade é positiva" ~: [Carro,Nenhum,Carro,Carro,Nenhum] ~=? animaObstaculoAux 1 obstaculoTeste04 0 ,
    "Teste para quando a velocidade é nula" ~: [Nenhum,Carro,Nenhum,Carro,Nenhum] ~=? animaObstaculoAux 0 obstaculoTeste05 0
    ]