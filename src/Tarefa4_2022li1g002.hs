{- |
Module      : Tarefa4_2022li1g002
Description : Determinar se o jogo terminou
Copyright   : João d'Araújo Dias Lobo <a104356@alunos.uminho.pt>
              Rita da Cunha Camacho <a104439@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g002 where

import LI12223

jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l p))
    | x < 0 || x >= length (snd (head p)) || y < 0 || y >= length p = True
    | otherwise = let terreno = fst (p !! y); obstaculo = snd (p !! y) !! x in
        case terreno of
            Relva -> False
            Estrada _ -> obstaculo == Carro
            Rio _ -> obstaculo == Nenhum