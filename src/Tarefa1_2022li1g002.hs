{- |
Module      : Tarefa1_2022li1g002
Description : Validação de um mapa
Copyright   : João d'Araújo Dias Lobo <a104356@alunos.uminho.pt>
              Rita da Cunha Camacho <a104439@alunos.uminho.pt>
 
Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g002 where
 
import LI12223
 
mapaValido :: Mapa -> Bool
mapaValido (Mapa _ []) = False
mapaValido (Mapa l t) = mapaValidoAnaliseLinhas (Mapa l t) && mapaValidoAnaliseGeral (Mapa l t)
 
mapaValidoAnaliseLinhas :: Mapa -> Bool
mapaValidoAnaliseLinhas (Mapa _ []) = True
mapaValidoAnaliseLinhas (Mapa l (h:t)) = 
    let linha = (snd h) in 
        (terrObsValidos h && quantObsValida l linha && existeNenhum linha && dimensObsValida linha) && mapaValidoAnaliseLinhas (Mapa l t)
 
mapaValidoAnaliseGeral :: Mapa -> Bool
mapaValidoAnaliseGeral (Mapa l (li)) = 
    let terrenos = (unzipperTerreno li) in 
        riosContiguosVelocidades terrenos && contaRios terrenos 0 && contaEstradas terrenos 0 && contaRelvas terrenos 0
 
terrObsValidos :: (Terreno, [Obstaculo]) -> Bool
terrObsValidos (_, []) = True
terrObsValidos (Rio v, h:t) = obstaculoEmTerrenoValido (Rio v) h && terrObsValidos (Rio v, t)
terrObsValidos (Estrada v, h:t) = obstaculoEmTerrenoValido (Estrada v) h && terrObsValidos (Estrada v, t)
terrObsValidos (Relva, h:t) = obstaculoEmTerrenoValido Relva h && terrObsValidos (Relva, t)
 
obstaculoEmTerrenoValido :: Terreno -> Obstaculo -> Bool
obstaculoEmTerrenoValido (Rio _) Tronco = True
obstaculoEmTerrenoValido (Rio _) Nenhum = True
obstaculoEmTerrenoValido (Estrada _) Carro = True
obstaculoEmTerrenoValido (Estrada _) Nenhum = True
obstaculoEmTerrenoValido Relva Arvore = True
obstaculoEmTerrenoValido Relva Nenhum = True
obstaculoEmTerrenoValido _ _ = False
 
riosContiguosVelocidades :: [Terreno] -> Bool
riosContiguosVelocidades [] = True
riosContiguosVelocidades ((Rio v1):(Rio v2):t) = velocidadesInversas (v1,v2) && (riosContiguosVelocidades ((Rio v2):t))
riosContiguosVelocidades (h:t) = riosContiguosVelocidades t
 
velocidadesInversas :: (Velocidade,Velocidade) -> Bool
velocidadesInversas (v1,v2) 
    | v1 * v2 < 0 = True
    | otherwise = False
 
dimensObsValida :: [Obstaculo] -> Bool
dimensObsValida l = (contaTroncos l 0) && (contaCarros l 0)
 
contaTroncos :: [Obstaculo] -> Int -> Bool
contaTroncos [] n = True
contaTroncos ((Tronco):t) n
    | n + 1 > 5 = False
    | otherwise = contaTroncos t (n+1)
contaTroncos (_:t) n = contaTroncos t 0
 
contaCarros :: [Obstaculo] -> Int -> Bool
contaCarros [] n = True
contaCarros ((Carro):t) n
    | n + 1 > 3 = False
    | otherwise = contaCarros t (n+1)
contaCarros (_:t) n = contaCarros t 0
 
existeNenhum :: [Obstaculo] -> Bool
existeNenhum [] = False
existeNenhum (h:t) =
    case h of
        Nenhum -> True
        _ -> existeNenhum t
 
quantObsValida :: Largura -> [Obstaculo] -> Bool
quantObsValida l o = length o == l
 
contaRios :: [Terreno] -> Int -> Bool
contaRios [] n = True
contaRios ((Rio _):t) n
    | n + 1 > 4 = False
    | otherwise = contaRios t (n+1)
contaRios (_:t) n = contaRios t 0
 
contaRelvas :: [Terreno] -> Int -> Bool
contaRelvas [] n = True
contaRelvas ((Relva):t) n
    | n + 1 > 5 = False
    | otherwise = contaRelvas t (n+1)
contaRelvas (_:t) n = contaRelvas t 0
 
contaEstradas :: [Terreno] -> Int -> Bool
contaEstradas [] n = True
contaEstradas ((Estrada _):t) n
    | n + 1 > 5 = False
    | otherwise = contaEstradas t (n+1)
contaEstradas (_:t) n = contaEstradas t 0
 
unzipperTerreno :: [(Terreno, [Obstaculo])] -> [Terreno]
unzipperTerreno [] = []
unzipperTerreno ((ts,_):t) = ts:unzipperTerreno t