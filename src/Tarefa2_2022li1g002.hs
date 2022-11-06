{- |
Module      : Tarefa2_2022li1g002
Description : Geração contínua de um mapa
Copyright   : João d'Araújo Dias Lobo <a104356@alunos.uminho.pt>
              Rita da Cunha Camacho <a104439@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g002 where

import LI12223

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa = undefined

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos = undefined

proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos l (terr, obs) =
    let cheio = length obs == l
        ultNenhum = (length obs) + 1 == l && not (elem Nenhum obs) in
        case terr of
            Relva -> if cheio then [] else 
                if ultNenhum then [Nenhum] else
                    [Nenhum, Arvore]
            Estrada _ -> if cheio then [] else
                if ultNenhum then [Nenhum] else
                    proximosObstaculosEstradaAux obs 0
            Rio _ -> if cheio then [] else 
                if ultNenhum then [Nenhum] else
                    proximosObstaculosRioAux obs 0
 
proximosObstaculosEstradaAux :: [Obstaculo] -> Int -> [Obstaculo]
proximosObstaculosEstradaAux _ 3 = [Nenhum]
proximosObstaculosEstradaAux [] _ = [Nenhum, Carro]
proximosObstaculosEstradaAux l n = if (last l == Carro) then proximosObstaculosEstradaAux (init l) (n+1) else [Nenhum, Carro]

proximosObstaculosRioAux :: [Obstaculo] -> Int -> [Obstaculo]
proximosObstaculosRioAux _ 5 = [Nenhum]
proximosObstaculosRioAux [] _ = [Nenhum, Tronco]
proximosObstaculosRioAux l n = if (last l == Tronco) then proximosObstaculosRioAux (init l) (n+1) else [Nenhum, Tronco]