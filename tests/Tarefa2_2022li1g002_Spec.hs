module Tarefa2_2022li1g002_Spec where

import LI12223
import Tarefa2_2022li1g002
import Tarefa1_2022li1g002
import Test.HUnit

mapaTeste01 = Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]
mapaTeste02 = Mapa 10 [(Relva, [Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum]),(Estrada (-3), [Nenhum, Nenhum, Nenhum, Carro, Carro, Carro, Nenhum, Nenhum, Nenhum, Nenhum]),(Estrada 5, [Nenhum, Carro, Carro, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Carro, Nenhum]),(Rio 6, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-4), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),(Rio 7, [Nenhum, Nenhum, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum])]

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test (testsEstendeMapa++testsGerarObstaculos++testsProximosTerrenosValidos++testsProximosObstaculosValidos++testsContaPrimeirasUltimasOcorrencias++testsUltimoObstaculoCaso)

{- 
NOTA: Para a segunda tarefa, como a função principal "estendeMapa" é aleatória, usamos a função criada na primeira tarefa 
"mapaValido" para verificar se a função funciona como estipulado no guião.
-}

testsEstendeMapa = [
    TestCase (assertBool "Teste para a extensão do exemplo dado na quarta página do primeiro guião com seed 52" (mapaValido (estendeMapa mapaTeste01 52))),
    TestCase (assertBool "Teste para a extensão de um mapa de onze linhas e vários obstáculos com seed 84" (mapaValido (estendeMapa mapaTeste01 84))),
    TestCase (assertBool "Teste para a extensão de um mapa vazio com seed 1" (mapaValido (estendeMapa (Mapa 10 []) 1)))
    ]

testsGerarObstaculos = [
    "Teste para a geração de 5 obstáculos num rio com seed 65" ~: [Nenhum,Tronco,Nenhum,Tronco,Nenhum] ~=? gerarObstaculos (obterRandoms 65 5) 5 (Rio (-1))  [],
    "Teste para a geração de 12 obstáculos numa estrada com seed 12" ~: [Nenhum,Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum] ~=? gerarObstaculos (obterRandoms 12 10) 10 (Estrada 2)  []
    ]

testsProximosTerrenosValidos = [
    "Teste para a verificar os terrenos válidos para a extensão do exemplo dado na quarta página do primeiro guião para a seed 43" ~: [Rio 2, Estrada 2, Relva] ~=? proximosTerrenosValidos mapaTeste01 (head (obterRandoms 43 1)),
    "Teste para verificar os terrenos válidos para a extensão de um mapa de onze linhas e vários obstáculos para a seed 22" ~: [Rio (-1),Estrada (-1)] ~=? proximosTerrenosValidos mapaTeste02 (head (obterRandoms 22 1))
    ]

testsProximosObstaculosValidos = [
    "Teste para um rio com 5 troncos seguidos (largura 7)" ~: [Nenhum] ~=? proximosObstaculosValidos 7 (Rio 1, [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco]),
    "Teste para uma estrada com 3 carros seguidos (largura 6)" ~: [Nenhum] ~=? proximosObstaculosValidos 6 (Estrada (-2), [Carro,Nenhum,Carro,Carro,Carro]),
    "Teste para relva com 5 árvores seguidas (largura 6)" ~: [Nenhum] ~=? proximosObstaculosValidos 6 (Relva, [Arvore,Arvore,Arvore,Arvore,Arvore]),
    "Teste para um rio genérico (largura 10)" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos 10 (Rio 2, [Tronco])
    ]

testsContaPrimeirasUltimasOcorrencias = [
    "Teste para uma lista de Int" ~: 3 ~=? contaPrimeirasUltimasOcorrencias [1,1,1,2,3,4,5] 1 True,
    "Teste para uma lista de String" ~: 2 ~=? contaPrimeirasUltimasOcorrencias ["teste","um","teste","teste"] "teste" False
    ]

testsUltimoObstaculoCaso = [
    "Teste para 3 carros no início" ~: [Nenhum] ~=? ultimoObstaculoCaso (Estrada 1) [Carro,Carro,Carro,Nenhum,Nenhum],
    "Teste para 5 troncos no inícRelvaio" ~: [Nenhum] ~=? ultimoObstaculoCaso (Rio (-1)) [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum],
    "Teste para uma lista de obstáculos vazia" ~: [Nenhum, Tronco] ~=? ultimoObstaculoCaso (Rio 1) []
    ]