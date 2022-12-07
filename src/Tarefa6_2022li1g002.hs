{- |
Module      : Tarefa6_2022li1g002
Description : ESCREVER DESCRIÇÃO
Copyright   : João d'Araújo Dias Lobo <a104356@alunos.uminho.pt>
              Rita da Cunha Camacho <a104439@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa6_2022li1g002 where

import LI12223
import Tarefa2_2022li1g002
import Tarefa3_2022li1g002
import Tarefa4_2022li1g002
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game

data Cena = MainMenu deriving (Eq, Show)

data Options = Jogar | Sair deriving Show

data Estado = Estado
    {
        cenaAtual :: Cena,
        optionAtual :: Options
    } deriving Show

estadoInicial :: Estado
estadoInicial = Estado MainMenu Jogar

dm :: Display
dm = InWindow "Monad Road" dimensoesDisplay (0,0) 

dimensoesDisplay :: (Int, Int)
dimensoesDisplay = (1600,900)

imagensCaminho :: String
imagensCaminho = "assets/images/"

fps :: Int
fps = 60

drawEstado :: [[Picture]] -> Estado -> Picture
drawEstado ps (Estado MainMenu Jogar) = head $ head ps 
drawEstado ps (Estado MainMenu Sair) = last $ head ps 

inputReage :: Event -> Estado -> Estado
inputReage (EventKey (SpecialKey KeyDown) Down _ _) estado@(Estado MainMenu Jogar) = estado{ optionAtual = Sair }
inputReage (EventKey (SpecialKey KeyUp) Down _ _) estado@(Estado MainMenu Sair) = estado{ optionAtual = Jogar }
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado MainMenu Sair) = undefined

inputReage _ e = e

tempoReage :: Float -> Estado -> Estado
tempoReage _ e = e

main :: IO()
main = do
    Just mm00 <- loadJuicyPNG $ imagensCaminho ++ "ui/mainmenu00.png"
    Just mm01 <- loadJuicyPNG $ imagensCaminho ++ "ui/mainmenu01.png"
    play dm 
        ((greyN 0.25))
        fps
        estadoInicial
        (drawEstado [[mm00, mm01]]) 
        inputReage
        tempoReage