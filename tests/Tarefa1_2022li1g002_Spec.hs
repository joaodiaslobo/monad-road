module Tarefa1_2022li1g002_Spec where

import LI12223
import Tarefa1_2022li1g002
import Test.HUnit

mapaTeste01 = Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]
mapaTeste02 = Mapa 10 [(Relva, [Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum]),(Estrada (-3), [Nenhum, Nenhum, Nenhum, Carro, Carro, Carro, Nenhum, Nenhum, Nenhum, Nenhum]),(Estrada 5, [Nenhum, Carro, Carro, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Carro, Nenhum]),(Rio 6, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-4), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),(Rio 7, [Nenhum, Nenhum, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum])]

linhaTeste01 = (Rio (-1),[Tronco, Tronco, Nenhum, Tronco])
linhaTeste02 = (Estrada (-1),[Tronco, Arvore, Nenhum, Tronco])

terrenosTeste01 = [Relva, Rio 3, Rio (-1), Rio 2, Estrada 2]
terrenosTeste02 = [Relva, Rio 3, Rio (-1), Rio 2, Rio 1]

obstaculosTeste01 = [Carro,Carro,Nenhum,Nenhum,Carro,Carro,Carro]
obstaculosTeste02 = [Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco]
obstaculosTeste03 = [Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco]

linhasTeste01 = [(Relva, [Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum]),(Estrada (-3), [Nenhum, Nenhum, Nenhum, Carro, Carro, Carro, Nenhum, Nenhum, Nenhum, Nenhum]),(Estrada 5, [Nenhum, Carro, Carro, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Carro, Nenhum]),(Rio 6, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-4), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),(Rio 7, [Nenhum, Nenhum, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum])]

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test (testsMapaValido++testsTerrObsValidos++testsRiosContiguosVelocidades++testsDimensObsValida++testsExisteNenhum++testsQuantObsValida++testsUnzipperTerreno)

testsMapaValido = [
    TestCase (assertBool "Teste para o exemplo dado na quarta p??gina do primeiro gui??o" (mapaValido mapaTeste01)),
    TestCase (assertBool "Teste para um mapa de onze linhas e v??rios obst??culos" (mapaValido mapaTeste02)),
    TestCase (assertBool "Teste para um mapa vazio" (not (mapaValido (Mapa 2 []))))
    ]

testsTerrObsValidos = [
    TestCase (assertBool "Teste para uma linha bem formada" (terrObsValidos linhaTeste01)),
    TestCase (assertBool "Teste para uma linha mal formada (obst??culos que n??o pertencem ao terreno)" (not (terrObsValidos linhaTeste02)))
    ]

testsRiosContiguosVelocidades = [
    TestCase (assertBool "Teste para uma lista de terrenos bem formada" (riosContiguosVelocidades terrenosTeste01)),
    TestCase (assertBool "Teste para uma lista de terrenos com rios contiguos de sentidos iguais" (not (riosContiguosVelocidades terrenosTeste02)))
    ]

testsDimensObsValida = [
    TestCase (assertBool "Teste para uma lista de obst??culos (carros) com as dimens??es corretas" (dimensObsValida obstaculosTeste01)),
    TestCase (assertBool "Teste para uma lista de obst??culos (troncos) com as dimens??es corretas" (dimensObsValida obstaculosTeste02)),
    TestCase (assertBool "Teste para uma lista de obst??culos (troncos) com as dimens??es erradas" (not (dimensObsValida obstaculosTeste03)))
    ]

testsExisteNenhum = [
    TestCase (assertBool "Teste para uma lista de obst??culos (carros) com nenhum" (existeNenhum obstaculosTeste01)),
    TestCase (assertBool "Teste para uma lista de obst??culos (troncos) sem nenhum" (not (existeNenhum obstaculosTeste03)))
    ]

testsQuantObsValida = [
    TestCase (assertBool "Teste para uma lista de obst??culos largura sete" (quantObsValida 7 obstaculosTeste01)),
    TestCase (assertBool "Teste para uma lista de obst??culos largura cinco" (not (quantObsValida 5 obstaculosTeste01)))
    ]

testsUnzipperTerreno = [
    "Teste para onze linhas" ~: [Relva,Estrada (-3),Estrada 5,Rio 6,Rio (-4),Rio 7,Relva,Relva,Relva,Relva,Relva] ~=? unzipperTerreno linhasTeste01,
    "Teste para uma lista vazia" ~: [] ~=? unzipperTerreno []
    ]