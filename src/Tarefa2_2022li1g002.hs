{- |
Module      : Tarefa2_2022li1g002
Description : Geração contínua de um mapa
Copyright   : João d'Araújo Dias Lobo <a104356@alunos.uminho.pt>
              Rita da Cunha Camacho <a104439@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g002 where

import LI12223
import System.Random ( mkStdGen, Random(randoms) )

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l to) seed =
    let randoms = obterRandoms seed (1+l)
        terreno = gerarTerreno (head randoms) (proximosTerrenosValidos (Mapa l to))
        obstaculos = gerarObstaculos (tail randoms) l terreno [] in
            Mapa l (to++[(terreno, obstaculos)])

gerarObstaculos :: [Int] 
    -> Int -- ^largura 
    -> Terreno
    -> [Obstaculo]
    -> [Obstaculo]
gerarObstaculos [] _ _ obs = obs 
gerarObstaculos (h:ss) l t o = let obsValidos = (proximosObstaculosValidos l (t, o)); obs = obsValidos !! (mod h (length obsValidos)) in
        gerarObstaculos ss l t o++[obs]

gerarTerreno :: Int -> [Terreno] -> Terreno
gerarTerreno s l = (l !! (mod s (length l)))

obterRandoms :: Int -> Int -> [Int]
obterRandoms seed n = take n $ randoms (mkStdGen seed)

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ l) = 
    let ultTerreno = fst (last l) in
        case ultTerreno of
            Rio _ -> rioAux (Mapa 0 l) 0
            Estrada _ -> estradaAux (Mapa 0 l) 0
            Relva -> relvaAux (Mapa 0 l) 0

rioAux :: Mapa -> Int -> [Terreno]
rioAux (Mapa _ []) n = [Rio 0, Estrada 0, Relva]
rioAux (Mapa _ l) n =
    let ultTerreno = fst (last l) in
        case ultTerreno of
            Rio _ -> 
                if n < 3 then rioAux (Mapa 0 (init l)) (n+1) 
                else [Estrada 0, Relva]
            _ -> [Rio 0, Estrada 0, Relva]

estradaAux :: Mapa -> Int -> [Terreno]
estradaAux (Mapa _ []) n = [Rio 0, Estrada 0, Relva]
estradaAux (Mapa _ l) n =
    let ultTerreno = fst (last l) in
        case ultTerreno of
            Estrada _ -> 
                if n < 4 then estradaAux (Mapa 0 (init l)) (n+1)
                else [Rio 0, Relva]
            _ -> [Rio 0, Estrada 0, Relva]

relvaAux :: Mapa -> Int -> [Terreno]
relvaAux (Mapa _ []) n = [Rio 0, Estrada 0, Relva]
relvaAux (Mapa _ l) n =
    let ultTerreno = fst (last l) in
        case ultTerreno of
            Relva ->
                if n < 4 then relvaAux (Mapa 10 (init l)) (n+1)
                else [Rio 0, Estrada 0]
            _ -> [Rio 0, Estrada 0, Relva]

proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos l (terr, obs) =
    let cheio = length obs == l
        ultNenhum = length obs + 1 == l && notElem Nenhum obs in
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
proximosObstaculosEstradaAux l n = if last l == Carro then proximosObstaculosEstradaAux (init l) (n+1) else [Nenhum, Carro]

proximosObstaculosRioAux :: [Obstaculo] -> Int -> [Obstaculo]
proximosObstaculosRioAux _ 5 = [Nenhum]
proximosObstaculosRioAux [] _ = [Nenhum, Tronco]
proximosObstaculosRioAux l n = if last l == Tronco then proximosObstaculosRioAux (init l) (n+1) else [Nenhum, Tronco]