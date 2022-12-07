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
import System.Random

data Cena = MainMenu | JogoCena deriving (Eq, Show)

data Options = Jogar | Sair deriving Show

data Estado = Estado
    {
        cenaAtual :: Cena,
        optionAtual :: Options,
        jogoAtual :: Jogo,
        seed :: Int
    } deriving Show

estadoInicial :: Estado
estadoInicial = Estado MainMenu Jogar (Jogo (Jogador (0,0)) (geraMapa 20 19 1)) 1

dm :: Display
dm = InWindow "Monad Road" dimensoesDisplay (0,0) 

dimensoesDisplay :: (Int, Int)
dimensoesDisplay = (1600,900)

imagensCaminho :: String
imagensCaminho = "assets/images/"

fps :: Int
fps = 60

drawEstado :: [[Picture]] -> Estado -> Picture
drawEstado ps (Estado MainMenu Jogar _ _) = head $ head ps 
drawEstado ps (Estado MainMenu Sair _ _) = last $ head ps 
drawEstado ps (Estado JogoCena _ j@(Jogo p m) s) = Pictures [drawJogo (ps !! 1) m,Translate (-785) 400 $ Scale 0.2 0.2 (Text ("SEED: "++show s))]

drawJogo :: [Picture] -> Mapa -> Picture
drawJogo ps m = Pictures $ drawMapa ps m (0,400)

drawMapa :: [Picture] -> Mapa -> (Float,Float) -> [Picture]
drawMapa _ (Mapa la []) _ = []
drawMapa ps m@(Mapa la ls) (x,y) = Pictures (drawLinha ps la (head ls) (x, y)): drawMapa ps (Mapa la $ tail ls) (x - 50, y - 25)

drawLinha :: [Picture] -> Largura -> (Terreno, [Obstaculo]) -> (Float,Float) -> [Picture]
drawLinha _ 0 _ _ = []
drawLinha ps lar (t, obs) (x,y) =
    case t of
        Relva -> drawSegmento (head ps) (x,y):drawLinha ps (lar - 1) (t,obs) (x+50,y-25)
        Rio _ -> drawSegmento (ps !! 1) (x,y):drawLinha ps (lar - 1) (t,obs) (x+50,y-25)
        Estrada _ -> drawSegmento (ps !! 2) (x,y):drawLinha ps (lar - 1) (t,obs) (x+50,y-25)

drawSegmento :: Picture -> (Float,Float) -> Picture
drawSegmento p (x,y) = Translate x y p


inputReage :: Event -> Estado -> Estado
inputReage (EventKey (SpecialKey KeyDown) Down _ _) estado@(Estado MainMenu Jogar _ _) = estado{ optionAtual = Sair }
inputReage (EventKey (SpecialKey KeyUp) Down _ _) estado@(Estado MainMenu Sair _ _) = estado{ optionAtual = Jogar }
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado MainMenu Sair _ _) = undefined
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado MainMenu Jogar _ _) = estado{ cenaAtual = JogoCena }
inputReage (EventKey (Char 'g') Down _ _) estado@(Estado JogoCena _ _ s) = estado{ jogoAtual = Jogo (Jogador (0,0)) (geraMapa 20 19 (s+1)), seed = 1+s }

inputReage _ e = e

tempoReage :: Float -> Estado -> Estado
tempoReage _ e = e

main :: IO()
main = do
    Just mm00 <- loadJuicyPNG $ imagensCaminho ++ "ui/mainmenu00.png"
    Just mm01 <- loadJuicyPNG $ imagensCaminho ++ "ui/mainmenu01.png"
    Just relva <- loadJuicyPNG $ imagensCaminho ++ "terreno/relva.png"
    Just rio <- loadJuicyPNG $ imagensCaminho ++ "terreno/rio.png"
    Just estrada <- loadJuicyPNG $ imagensCaminho ++ "terreno/estrada.png"
    play dm 
        (greyN 0.25)
        fps
        estadoInicial
        (drawEstado [[mm00, mm01], [relva,rio,estrada]]) 
        inputReage
        tempoReage

{- | A função __não recursiva__ 'geraMapa', através da função auxiliar 'geraMapaAux' e da função 'obterRandoms' presente no módulo da tarefa 2, gera um mapa pseudo-aleatório utilizando uma /seed/ e uma largura, ambas inseridas pelo utilizador.-}
geraMapa :: Largura -- ^Largura do mapa a ser gerado.
    -> Int -- ^Altura do mapa.
    -> Int -- ^/Seed/.
    -> Mapa -- ^Mapa gerado.
geraMapa l a r = let randoms = obterRandoms r a in geraMapaAux randoms l

{- | A função __recursiva__ 'geraMapaAux' cria linha a linha o mapa a ser gerado, tendo em conta que a primeira linha será relva, sem nenhum obstáculo.-}
geraMapaAux :: [Int] -- ^Lista de /randoms/.
    -> Largura -- ^Largura do mapa a ser gerado.
    -> Mapa -- ^Mapa gerado.
geraMapaAux [] l = Mapa l [(Relva, replicate l Nenhum)] 
geraMapaAux (h:t) l = let ss = mod h 100 in estendeMapa (geraMapaAux t l) ss