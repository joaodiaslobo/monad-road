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
mapaValido (Mapa l t) = mapaValidoAnaliseLinhas (Mapa l t)

mapaValidoAnaliseLinhas :: Mapa -> Bool
mapaValidoAnaliseLinhas (Mapa _ []) = True
mapaValidoAnaliseLinhas (Mapa l (h:t)) = 
    let linha = (snd h) in 
        (terrObsValidos h && quantObsValida l linha && existeNenhum linha && dimensObsValida linha) && mapaValidoAnaliseLinhas (Mapa l t)

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

