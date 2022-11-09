{- |
Module      : Tarefa3_2022li1g002
Description : Movimentação do personagem e obstáculos
Copyright   : João d'Araújo Dias Lobo <a104356@alunos.uminho.pt>
              Rita da Cunha Camacho <a104439@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g002 where

import LI12223

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo p (Mapa l ls)) j = Jogo jogador (Mapa l linhas)
    where jogador = moveJogador p j ls; linhas = animaObstaculos ls

moveJogador :: Jogador -> Jogada -> [(Terreno, [Obstaculo])] -> Jogador
moveJogador (Jogador (x,y)) j p =
    case j of
        Parado ->
            case fst (p !! y) of
                Rio v -> if (snd (p !! y) !! x) == Tronco then Jogador (x+v,y) else Jogador (x,y)
                _ -> Jogador (x,y)
        Move Cima -> if  (y-1 < 0) || ((snd (p !! (y-1)) !! x) == Arvore) then Jogador (x,y) else Jogador (x,y-1)
        Move Baixo -> if (y+1 >= length p) || ((snd (p !! (y+1)) !! x) == Arvore) then Jogador (x,y) else Jogador (x,y+1)
        Move Direita -> if (x+1 >= length (snd (head p))) || (snd (p !! y) !! (x+1)) == Arvore then Jogador (x,y) else Jogador (x+1,y)
        Move Esquerda -> if (x-1 < 0) || (snd (p !! y) !! (x-1)) == Arvore then Jogador (x,y) else Jogador (x-1,y)

animaObstaculos :: [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
animaObstaculos [] = []
animaObstaculos (l:t) = 
    case fst l of
        Relva -> l:animaObstaculos t
        Rio v -> (Rio v, animaObstaculoAux v (snd l) 0):animaObstaculos t
        Estrada v -> (Estrada v, animaObstaculoAux v (snd l) 0):animaObstaculos t

animaObstaculoAux :: Velocidade -> [Obstaculo] -> Int -> [Obstaculo]
animaObstaculoAux v o n
    | v > 0 =
        if n < v then animaObstaculoAux v (last o:init o) (n+1) else o
    | v < 0 =
        if n < abs v then animaObstaculoAux v (tail o++[head o]) (n+1) else o