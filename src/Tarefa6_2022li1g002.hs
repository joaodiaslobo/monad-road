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
import Tarefa5_2022li1g002 (deslizaJogo)

data Cena = MainMenu | JogoCena deriving (Eq, Show)

data Options = Jogar | Sair deriving Show

data Estado = Estado
    {
        debug :: Bool,
        cena :: Cena,
        opcao :: Options,
        jogo :: Jogo,
        movimento :: Jogada,
        direcao :: Direcao,
        vivo :: Bool,
        tick :: Int,
        seed :: Int,
        pontuacao :: (Int,Int),
        genY :: Int
    } deriving Show

alturaMapa :: Int
alturaMapa = 30

larguraMapa :: Int
larguraMapa = 20

estadoInicial :: IO Estado
estadoInicial = do
    seed <- randomRIO (1 :: Int, 100 :: Int)
    return $ Estado False MainMenu Jogar (Jogo (Jogador (0,alturaMapa)) (geraMapa larguraMapa alturaMapa 1)) Parado Cima True 0 seed (0,0) (alturaMapa + 4)

dm :: Display
dm = InWindow "Monad Road" dimensoesDisplay (0,0)

dimensoesDisplay :: (Int, Int)
dimensoesDisplay = (1600,900)

imagensCaminho :: String
imagensCaminho = "assets/images/"

fps :: Int
fps = 30

taxaUpdate :: Int
taxaUpdate = 50

drawEstado :: [[Picture]] -> Estado -> Picture
drawEstado ps (Estado _ MainMenu Jogar _ _ _ _ _ _ _ _) = head $ head ps
drawEstado ps (Estado _ MainMenu Sair _ _ _ _ _ _ _ _) = head ps !! 1
drawEstado ps (Estado debug JogoCena _ j@(Jogo p@(Jogador c@(x,y)) m) _ d vivo t s (pt,_) gy)
    | debug = Pictures [drawJogo [ps !! 1,ps !! 2,ps !! 3] m c d t (pt >= 4 && vivo), drawMuros,drawDebugUI s (x,y) t d (not vivo) gy, drawJogoUI pt]
    | otherwise =  Pictures [drawJogo [ps !! 1,ps !! 2, ps !! 3] m c d t (pt >= 4 && vivo), drawMuros, drawJogoUI pt]

drawJogoUI :: Int -> Picture
drawJogoUI p = Translate 0 360 $ Scale 0.5 0.5 $ Text $ show p

drawDebugUI :: Int -> (Int,Int) -> Int -> Direcao -> Bool -> Int -> Picture
drawDebugUI s (x,y) t d m gy = Pictures [Translate (-785) 400 $ Scale 0.2 0.2 (Text ("SEED: "++show s)), Translate (-785) 350 $ Scale 0.2 0.2 (Text ("POSICAO: "++show x++", "++show y)), Translate (-785) 300 $ Scale 0.2 0.2 (Text ("DIRECAO: "++show d)), Translate (-785) 250 $ Scale 0.2 0.2 (Text ("MORTO?: "++show m)), Translate (-785) 200 $ Scale 0.2 0.2 (Text ("TICK: "++show t))]

drawMuros :: Picture
drawMuros = Pictures [Translate 700 (-465) $ Rotate 64 $ Color corFundo $ rectangleSolid 150 400, Translate (-400) 300 $ Rotate 64 $ Color corFundo $ rectangleSolid 150 400]

drawJogo :: [[Picture]] -> Mapa -> Coordenadas -> Direcao -> Int -> Bool -> Picture
drawJogo ps m@(Mapa l ls) co@(x,y) d t desliza = Translate (fromIntegral t * dx) (fromIntegral t * dy) $ Translate (fromIntegral alturaMapa * 50 - 900) (fromIntegral alturaMapa * 25 - 600) $ Pictures $ drawMapa (head ps) m (0,400)++drawObstaculos (ps !! 1) m (0,400) t co++[drawJogador (ps !! 2) d co v t]
    where v =
            let linha = ls !! y in
            case fst linha of
                Rio vv -> if x >= 0 && x <= l && snd linha !! x == Tronco then vv else 0
                _ -> 0
          (dx, dy) = if desliza then (-1, -0.5) else (0, 0)

drawJogador :: [Picture] -> Direcao -> Coordenadas -> Velocidade -> Int -> Picture
drawJogador ps d (x,y) 0 _ =
    case d of
        Cima -> Translate  (fromIntegral (x * 50 + y * (-50))) (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! 0)
        Baixo -> Translate (fromIntegral (x * 50 + y * (-50)))  (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! 1)
        Esquerda -> Translate (fromIntegral (x * 50 + y * (-50)))  (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! 2)
        Direita -> Translate (fromIntegral (x * 50 + y * (-50))) (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! 3)
drawJogador ps d (x,y) v t =
    case d of
        Cima -> Translate (fromIntegral $ t * v) (fromIntegral (t * v) * (- 0.5)) $ Translate  (fromIntegral (x * 50 + y * (-50))) (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! 0)
        Baixo -> Translate (fromIntegral $ t * v) (fromIntegral (t * v) * (- 0.5)) $ Translate (fromIntegral (x * 50 + y * (-50)))  (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! 1)
        Esquerda -> Translate (fromIntegral $ t * v) (fromIntegral (t * v) * (- 0.5)) $ Translate (fromIntegral (x * 50 + y * (-50)))  (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! 2)
        Direita -> Translate (fromIntegral $ t * v) (fromIntegral (t * v) * (- 0.5)) $ Translate (fromIntegral (x * 50 + y * (-50))) (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! 3)

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

drawObstaculos :: [Picture] -> Mapa -> (Float,Float) -> Int -> Coordenadas -> [Picture]
drawObstaculos _ (Mapa la []) _ _ _ = []
drawObstaculos ps m@(Mapa la ls) (x,y) t cordsJogador@(xx,yy) = let deveEstarParado = yy == 0 && (snd (head ls) !! xx) == Carro in
    Pictures (drawLinhaObstaculo ps (snd (head ls)) (x, y) v t deveEstarParado): drawObstaculos ps (Mapa la $ tail ls) (x - 50, y - 25) t (xx, yy-1)
    where v = case fst (head ls) of
                Rio vv -> vv
                Estrada vv -> vv
                Relva -> 0

drawLinhaObstaculo :: [Picture] -> [Obstaculo] -> (Float,Float) -> Velocidade -> Int -> Bool -> [Picture]
drawLinhaObstaculo _ [] _ _ _ _ = []
drawLinhaObstaculo ps obs@(h:t) (x,y) v ti p =
    case h of
        Arvore -> drawSegmento (head ps) (x,y+10):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p
        Tronco ->  Translate (fromIntegral $ ti * v) (fromIntegral (ti * v) * (- 0.5)) (drawSegmento (ps !! 1) (x,y)):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p
        Carro -> if v > 0 then if not p then Translate (fromIntegral $ ti * v) (fromIntegral (ti * v) * (- 0.5)) (drawSegmento (ps !! 2) (x,y)):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p else (drawSegmento (ps !! 2) (x,y)):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p
        else if v < 0 then if not p then Translate (fromIntegral $ ti * v) (fromIntegral (ti * v) * (- 0.5)) (drawSegmento (ps !! 3) (x,y)):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p else drawSegmento (ps !! 3) (x,y):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p
        else drawSegmento (ps !! 2) (x,y):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p
        Nenhum -> drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p

inputReage :: Event -> Estado -> Estado
inputReage (EventKey (Char 'p') Down _ _) estado@(Estado d _ _ _ _ _ _ _ _ _ _) = estado{ debug = not d }
inputReage (EventKey (SpecialKey KeyDown) Down _ _) estado@(Estado _ MainMenu Jogar _ _ _ _ _ _ _ _) = estado{ opcao = Sair }
inputReage (EventKey (SpecialKey KeyUp) Down _ _) estado@(Estado _ MainMenu Sair _ _ _ _ _ _ _ _) = estado{ opcao = Jogar }
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ MainMenu Sair _ _ _ _ _ _ _ _) = undefined
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ MainMenu Jogar _ _ _ _ _ _ _ _) = estado{ cena = JogoCena }
inputReage (EventKey (Char 'g') Down _ _) estado@(Estado _ JogoCena _ _ _ _ _ _ s _ _) = estado{ jogo = Jogo (Jogador (0,alturaMapa)) (geraMapa larguraMapa alturaMapa (s+1)), seed = 1+s, tick = 0, vivo = True }
inputReage (EventKey (Char 'a') Down _ _) estado@(Estado _ JogoCena _ _ _ _ _ t _ _ _) = estado{ movimento = Move Esquerda, direcao = Esquerda }
inputReage (EventKey (Char 'w') Down _ _) estado@(Estado _ JogoCena _ _ _ _ _ t _ _ _) = estado{ movimento = Move Cima, direcao = Cima }
inputReage (EventKey (Char 's') Down _ _) estado@(Estado _ JogoCena _ _ _ _ _ t _ _ _) = estado{ movimento = Move Baixo, direcao = Baixo }
inputReage (EventKey (Char 'd') Down _ _) estado@(Estado _ JogoCena _ _ _ _ _ t _ _ _) = estado{ movimento = Move Direita, direcao = Direita }
inputReage _ e = e

tempoReage :: Float -> Estado -> Estado
tempoReage f estado@(Estado _ JogoCena _ jogo@(Jogo j@(Jogador (x,y)) m@(Mapa _ ls)) movimento _ vivo t seed (pt,auxPt) gy)
    | t == taxaUpdate && vivo = let novoJogo = animaJogo jogo Parado; desliza = pt >= 4 in estado{ jogo = if not desliza then novoJogo else deslizaJogo (randoms (mkStdGen seed) !! gy)  novoJogo, tick = 0, vivo = not $ jogoTerminou novoJogo, genY = if desliza then gy + 1 else gy}
    | t == taxaUpdate = estado{tick = 0, jogo = jogo}
    | movimento /= Parado = let jogador@(Jogador (xx,yy)) = if t > div taxaUpdate 2 then moveJogador j movimento $ arranjaRios (obterLinhas $ animaJogo jogo Parado) else moveJogador j movimento $ arranjaRios ls; novoJogo = Jogo jogador m; novoAuxPt = if yy < y then auxPt + 1 else if yy > y then auxPt - 1 else auxPt; in
        estado{ jogo = novoJogo, movimento = Parado, tick = t+1, vivo = not $ jogoTerminou novoJogo, pontuacao = if novoAuxPt > 0 then (pt + 1,0) else (pt,novoAuxPt)}
    | otherwise = estado{ tick = t+1 }
tempoReage _ estado = estado

corFundo :: Color
corFundo = makeColor (79/255) (112/255) (126/255) 1

main :: IO()
main = do
    Just mm00 <- loadJuicyPNG $ imagensCaminho ++ "ui/mainmenu00.png"
    Just mm01 <- loadJuicyPNG $ imagensCaminho ++ "ui/mainmenu01.png"
    Just relva <- loadJuicyPNG $ imagensCaminho ++ "terreno/relva.png"
    Just rio <- loadJuicyPNG $ imagensCaminho ++ "terreno/rio.png"
    Just estrada <- loadJuicyPNG $ imagensCaminho ++ "terreno/estrada.png"
    Just arvore <- loadJuicyPNG $ imagensCaminho ++ "obstaculos/arvore.png"
    Just tronco <- loadJuicyPNG $ imagensCaminho ++ "obstaculos/tronco.png"
    Just carroDireita <- loadJuicyPNG $ imagensCaminho ++ "obstaculos/carroDireita.png"
    Just carroEsquerda <- loadJuicyPNG $ imagensCaminho ++ "obstaculos/carroEsquerda.png"
    Just galinhaCima <- loadJuicyPNG $ imagensCaminho ++ "jogador/galinhaCima.png"
    Just galinhaBaixo <- loadJuicyPNG $ imagensCaminho ++ "jogador/galinhaBaixo.png"
    Just galinhaEsquerda <- loadJuicyPNG $ imagensCaminho ++ "jogador/galinhaEsquerda.png"
    Just galinhaDireita <- loadJuicyPNG $ imagensCaminho ++ "jogador/galinhaDireita.png"

    seed <- randomRIO (1 :: Int, 100 :: Int)
    estado <- estadoInicial

    play dm
        corFundo
        fps
        estado
        (drawEstado [[mm00, mm01], [relva,rio,estrada], [arvore, tronco, carroDireita, carroEsquerda], [galinhaCima, galinhaBaixo, galinhaEsquerda, galinhaDireita]])
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
geraMapaAux [] l = Mapa l [(Relva, replicate l Nenhum),(Relva, replicate l Nenhum),(Relva, replicate l Nenhum),(Relva, replicate l Nenhum)]
geraMapaAux (h:t) l = let ss = mod h 100 in estendeMapa (geraMapaAux t l) ss

{- | Como na primeira parte do projeto o jogo foi feito a pensar no movimento do jogador sincronizado com a animação dos obstáculos, ao contrário da segunda parte (jogador move-se independente da animação dos obstáculos), precisamos de fazer uma correção ao movimento dos troncos para o jogador se conseguir mover. -}
arranjaRios :: [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
arranjaRios [] = []
arranjaRios ((t,obs):tt) =
    case t of
        Rio v -> (Rio 0, obs):arranjaRios tt
        _ -> (t,obs):arranjaRios tt

obterLinhas :: Jogo -> [(Terreno, [Obstaculo])]
obterLinhas (Jogo _ (Mapa _ ls)) = ls