{- |
Module      : Tarefa5_2022li1g002
Description : Executar o deslizamento do mapa do jogo
Copyright   : João d'Araújo Dias Lobo <a104356@alunos.uminho.pt>
              Rita da Cunha Camacho <a104439@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g002 where

import LI12223
import Tarefa2_2022li1g002 (estendeMapa)

{- | A função 'deslizaJogo' permite que o jogo vá avançando a nível de linhas, sendo o mapa renovado, eliminando a linha mais abaixo e gerando uma nova no topo do mapa, sucessivamente. -}
deslizaJogo :: Int -- ^Seed. 
    -> Jogo -- ^Jogo que irá ser deslizado.
    -> Jogo -- ^Jogo depois de ser deslizado, retirando a linha mais abaixo e adicionando uma nova no topo.
deslizaJogo s (Jogo (Jogador (x,y)) (Mapa larg l)) = Jogo (Jogador (x,y+1)) (estendeMapa (Mapa larg (init l)) s)