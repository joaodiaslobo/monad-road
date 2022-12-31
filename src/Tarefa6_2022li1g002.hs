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
import Tarefa5_2022li1g002
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import System.Directory
import Data.List
import System.IO

data Cena = MainMenu Int | JogoCena | Editor deriving (Eq, Show)

data Options = Jogar | Sair deriving Show

data Estado = Estado
    {
        debug :: Bool,
        cena :: Cena,
        jogo :: Jogo,
        movimento :: Jogada,
        direcao :: Direcao,
        vivo :: Bool,
        tick :: Int,
        seed :: Int,
        pontuacao :: (Int,Int),
        genY :: Int,
        editor :: EstadoEditor
    } deriving Show

alturaMapa :: Int
alturaMapa = 30

larguraMapa :: Int
larguraMapa = 20

estadoInicial :: IO Estado
estadoInicial = do
    seed <- randomRIO (1 :: Int, 100 :: Int)
    return $ Estado False (MainMenu 0) (Jogo (Jogador (0,alturaMapa)) (geraMapa larguraMapa alturaMapa 1)) Parado Cima True 0 seed (0,0) (alturaMapa + 4) estadoEditorInicial

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

drawEstado :: [[Picture]] -> Estado -> IO Picture
drawEstado ps (Estado _ (MainMenu m) _ _ _ _ _ _ _ _ _) = return $ drawMenu (MainMenu m) ps
drawEstado ps (Estado debug JogoCena j@(Jogo p@(Jogador c@(x,y)) m) _ d vivo t s (pt,_) gy _)
    | debug = return $ Pictures [drawJogo [ps !! 1,ps !! 2,ps !! 3] m c d t (pt >= 4 && vivo), drawMuros,drawDebugUI s (x,y) t d (not vivo) gy, drawJogoUI pt]
    | otherwise = return $ Pictures [drawJogo [ps !! 1,ps !! 2, ps !! 3] m c d t (pt >= 4 && vivo), drawMuros, drawJogoUI pt]
drawEstado ps (Estado _ Editor _ _ _ _ _ _ _ _ estadoEditor) = do
    imagensEditor <- editorDeMapasImagens
    return $ desenhaEditor estadoEditor [(ps !! 1),(ps !! 2)] imagensEditor

drawMenu :: Cena -> [[Picture]] -> Picture
drawMenu menu ps =
    case menu of
        MainMenu n -> head ps !! n

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

inputReage :: Event -> Estado -> IO Estado

-- INPUT DEBUG
inputReage (EventKey (Char 'l') Down _ _) estado@(Estado d _ _ _ _ _ _ _ _ _ _) = return estado{ debug = not d }

-- INPUT MAIN MENU
inputReage (EventKey (SpecialKey KeyDown) Down _ _) estado@(Estado _ (MainMenu 0) _ _ _ _ _ _ _ _ _) = return estado{ cena = MainMenu 1 }
inputReage (EventKey (SpecialKey KeyUp) Down _ _) estado@(Estado _ (MainMenu 1) _ _ _ _ _ _ _ _ _) = return estado{ cena = MainMenu 0 }
inputReage (EventKey (SpecialKey KeyDown) Down _ _) estado@(Estado _ (MainMenu 1) _ _ _ _ _ _ _ _ _) = return estado{ cena = MainMenu 2 }
inputReage (EventKey (SpecialKey KeyUp) Down _ _) estado@(Estado _ (MainMenu 2) _ _ _ _ _ _ _ _ _) = return estado{ cena = MainMenu 1 }
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ (MainMenu 2) _ _ _ _ _ _ _ _ _) = do
    putStr "Jogo encerrado."
    exitSuccess
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ (MainMenu 1) _ _ _ _ _ _ _ _ _) = return estado{ cena = Editor }
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ (MainMenu 0) _ _ _ _ _ _ _ _ _) = return estado{ cena = JogoCena }

-- INPUT JOGO PRINCIPAL
inputReage (EventKey (Char 'g') Down _ _) estado@(Estado _ JogoCena _ _ _ _ _ s _ _ _) = return estado{ jogo = Jogo (Jogador (0,alturaMapa)) (geraMapa larguraMapa alturaMapa (s+1)), seed = 1+s, tick = 0, vivo = True }
inputReage (EventKey (SpecialKey KeyLeft)  Down _ _) estado@(Estado _ JogoCena _ _ _ _ t _ _ _ _) = return estado{ movimento = Move Esquerda, direcao = Esquerda }
inputReage (EventKey (SpecialKey KeyUp) Down _ _) estado@(Estado _ JogoCena _ _ _ _ t _ _ _ _) = return estado{ movimento = Move Cima, direcao = Cima }
inputReage (EventKey (SpecialKey KeyDown) Down _ _) estado@(Estado _ JogoCena _ _ _ _ t _ _ _ _) = return estado{ movimento = Move Baixo, direcao = Baixo }
inputReage (EventKey (SpecialKey KeyRight)  Down _ _) estado@(Estado _ JogoCena _ _ _ _ t _ _ _ _) = return estado{ movimento = Move Direita, direcao = Direita }

-- INPUT EDITOR DE MAPAS
inputReage (EventKey (Char 'w') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ (x,y) False _ (False, _) _)) = return estado{ editor = editor{objeto = (x, proximoN y (alturaMapaEditor - 1) (-1)) }}
inputReage (EventKey (Char 's') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ (x,y) False _ (False, _) _)) = return estado{ editor = editor{objeto = (x, proximoN y (alturaMapaEditor - 1) 1) }}
inputReage (EventKey (Char 'd') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ (x,y) False _ (False, _) _)) = return estado{ editor = editor{objeto = (proximoN x (larguraMapaEditor - 1) 1, y) }}
inputReage (EventKey (Char 'a') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ (x,y) False _ (False, _) _)) = return estado{ editor = editor{objeto = (proximoN x (larguraMapaEditor - 1) (-1), y) }}
inputReage (EventKey (SpecialKey KeySpace)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor chunks _ _ tool item coord False _ (False, _) _)) = return estado{ editor = editor{ chunks = editaChunk chunks coord tool item } }
inputReage (EventKey (SpecialKey KeyEsc)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ False _ (False, _) _)) = return estado{ editor = estadoEditorInicial, cena = MainMenu 0 }
inputReage (EventKey (SpecialKey KeyEsc)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ True _ _ _)) = return estado{ editor = editor{ receberInput = False, nomeFicheiro = ""} }
inputReage (EventKey (SpecialKey KeyEsc)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ False _ (True, _) _)) = return estado{ editor = editor{ receberSelectMapa = (False, 0), mapasGuardados = []} }
inputReage (EventKey (Char '+')  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ (cx,cy,scale) _ _ _ False _ (False, _) _)) = return estado{ editor = editor{ camera = (cx, cy, scale + scale / 2)} }
inputReage (EventKey (Char '-')  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ (cx,cy,scale) _ _ _ False _ (False, _) _)) = return estado{ editor = editor{ camera = (cx, cy, scale - scale / 2)} }
inputReage (EventKey (SpecialKey KeyRight)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ n _ _ False _ (False, _) _)) = return estado{ editor = editor{ toolbar = proximoN n 1 1, itemSelecionado = 0 } }
inputReage (EventKey (SpecialKey KeyLeft) Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ n _ _ False _ (False, _) _)) = return estado{ editor = editor{ toolbar = proximoN n 1 1, itemSelecionado = 0} }
inputReage (EventKey (SpecialKey KeyUp)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ n _ False _ (False, _) _)) = return estado{ editor = editor{ itemSelecionado = proximoN n 2 (-1) } }
inputReage (EventKey (SpecialKey KeyDown) Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ n _ False _ (False, _) _)) = return estado{ editor = editor{ itemSelecionado = proximoN n 2 1} }
inputReage (EventKey (Char c) Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ True t (False, _) _)) = return estado{ editor = editor{ nomeFicheiro = t++[c]}}
inputReage (EventKey (SpecialKey KeyBackspace) Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ True t (False, _) _)) = return estado{ editor = editor{ nomeFicheiro = drop 1 t}}
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ True t (False, _) _)) = return estado{ editor = editor{ receberInput = False }}
inputReage (EventKey (Char 'g') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor chunks _ _ _ _ _ False _ (False, _) _)) = return estado{ editor = editor{ receberInput = True }}
inputReage (EventKey (Char 'c') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor chunks _ _ _ _ _ False _ (False, _) _)) =
    do
        nomeMapas <- nomeMapasGuardados
        return estado{ editor = editor{ receberSelectMapa = (True, 0), mapasGuardados = nomeMapas}}
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ False _ (True, n) _)) = return estado{ editor = editor{ receberSelectMapa = (False, n) }}
inputReage (EventKey (SpecialKey KeyUp)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ n _ False _ (True, index) ns)) = return estado{ editor = editor{ receberSelectMapa = (True, proximoN index (length ns - 1) (-1)) } }
inputReage (EventKey (SpecialKey KeyDown)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ n _ False _ (True, index) ns)) = return estado{ editor = editor{ receberSelectMapa = (True, proximoN index (length ns - 1) (1)) } }
inputReage (EventKey (Char 'p') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor chunks _ _ _ _ _ False _ (False, _) _)) = if chunksProntos chunks then return estado{ jogo = Jogo (Jogador (0,alturaMapaEditor-1)) (chunksParaMapa chunks), cena = JogoCena, editor = estadoEditorInicial } else return estado

inputReage _ e = return e

proximoN :: Int -> Int -> Int -> Int
proximoN c max i
  | c + i > max = 0
  | c + i < 0 = max
  | otherwise = c + i

tempoReage :: Float -> Estado -> IO Estado
tempoReage f estado@(Estado _ JogoCena jogo@(Jogo j@(Jogador (x,y)) m@(Mapa _ ls)) movimento _ vivo t seed (pt,auxPt) gy _)
    | t == taxaUpdate && vivo = let novoJogo = animaJogo jogo Parado; desliza = pt >= 4 in return estado{ jogo = if not desliza then novoJogo else deslizaJogo (randoms (mkStdGen seed) !! gy)  novoJogo, tick = 0, vivo = not $ jogoTerminou novoJogo, genY = if desliza then gy + 1 else gy}
    | t == taxaUpdate = return estado{tick = 0, jogo = jogo}
    | movimento /= Parado = let jogador@(Jogador (xx,yy)) = if t > div taxaUpdate 2 then moveJogador j movimento $ arranjaRios (obterLinhas $ animaJogo jogo Parado) else moveJogador j movimento $ arranjaRios ls; novoJogo = Jogo jogador m; novoAuxPt = if yy < y then auxPt + 1 else if yy > y then auxPt - 1 else auxPt; in
        return estado{ jogo = novoJogo, movimento = Parado, tick = t+1, vivo = not $ jogoTerminou novoJogo, pontuacao = if novoAuxPt > 0 then (pt + 1,0) else (pt,novoAuxPt)}
    | otherwise = return estado{ tick = t+1 }
tempoReage _ estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor chunks _ _ _ _ _ False t (False, s) ns))
    | t /= "" = do
        writeFile (t++".mapa") (show chunks)
        putStr "O mapa que estava a ser editado foi gravado"
        return estado{ editor = editor{ receberInput = False, nomeFicheiro = "" }}
    | ns /= [] = do
        nome <- readFile (ns !! s)
        novosChunks <- leChunks nome
        return estado{ editor = editor{ receberSelectMapa = (False,0), mapasGuardados = [], chunks = novosChunks }}
    | otherwise = return estado
tempoReage _ estado = return estado

corFundo :: Color
corFundo = makeColor (79/255) (112/255) (126/255) 1

main :: IO()
main = do
    -- Main Menu
    Just mm00 <- loadJuicyPNG $ imagensCaminho ++ "ui/mainmenu00.png"
    Just mm01 <- loadJuicyPNG $ imagensCaminho ++ "ui/mainmenu01.png"
    Just mm02 <- loadJuicyPNG $ imagensCaminho ++ "ui/mainmenu02.png"
    -- Terrenos
    Just relva <- loadJuicyPNG $ imagensCaminho ++ "terreno/relva.png"
    Just rio <- loadJuicyPNG $ imagensCaminho ++ "terreno/rio.png"
    Just estrada <- loadJuicyPNG $ imagensCaminho ++ "terreno/estrada.png"
    Just nenhum <- loadJuicyPNG $ imagensCaminho ++ "terreno/nenhum.png"
    -- Obstáculos
    Just arvore <- loadJuicyPNG $ imagensCaminho ++ "obstaculos/arvore.png"
    Just tronco <- loadJuicyPNG $ imagensCaminho ++ "obstaculos/tronco.png"
    Just carroDireita <- loadJuicyPNG $ imagensCaminho ++ "obstaculos/estrada/carroDireita.png"
    Just carroEsquerda <- loadJuicyPNG $ imagensCaminho ++ "obstaculos/estrada/carroEsquerda.png"
    -- Jogador
    Just galinhaCima <- loadJuicyPNG $ imagensCaminho ++ "jogador/galinhaCima.png"
    Just galinhaBaixo <- loadJuicyPNG $ imagensCaminho ++ "jogador/galinhaBaixo.png"
    Just galinhaEsquerda <- loadJuicyPNG $ imagensCaminho ++ "jogador/galinhaEsquerda.png"
    Just galinhaDireita <- loadJuicyPNG $ imagensCaminho ++ "jogador/galinhaDireita.png"

    seed <- randomRIO (1 :: Int, 100 :: Int)
    estado <- estadoInicial

    playIO dm
        corFundo
        fps
        estado
        (drawEstado [[mm00, mm01, mm02], [relva,rio,estrada,nenhum], [arvore, tronco, carroDireita, carroEsquerda], [galinhaCima, galinhaBaixo, galinhaEsquerda, galinhaDireita]])
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

-- =============== EDITOR DE MAPAS ===============

data Chunk = Linha (Terreno, [Obstaculo]) | Nada deriving (Show, Read)

type CameraEditor = (Int, Int, Float)

editorDeMapasImagens :: IO [[Picture]]
editorDeMapasImagens = do
    Just toolbarTerrenosRelva <- loadJuicyPNG $ imagensCaminho ++ "ui/editor/editor_toolbar_terrenos_relva.png"
    Just toolbarTerrenosEstrada <- loadJuicyPNG $ imagensCaminho ++ "ui/editor/editor_toolbar_terrenos_estrada.png"
    Just toolbarTerrenosRio <- loadJuicyPNG $ imagensCaminho ++ "ui/editor/editor_toolbar_terrenos_rio.png"
    Just toolbarObstaculosArvore <- loadJuicyPNG $ imagensCaminho ++ "ui/editor/editor_toolbar_obstaculos_arvore.png"
    Just toolbarObstaculosCarro <- loadJuicyPNG $ imagensCaminho ++ "ui/editor/editor_toolbar_obstaculos_carro.png"
    Just toolbarObstaculosTronco <- loadJuicyPNG $ imagensCaminho ++ "ui/editor/editor_toolbar_obstaculos_tronco.png"
    Just tooltips <- loadJuicyPNG $ imagensCaminho ++ "ui/editor/tooltips.png"
    Just cursor <- loadJuicyPNG $ imagensCaminho ++ "ui/editor/objeto_selecionado.png"
    Just caixaInput <- loadJuicyPNG $ imagensCaminho ++ "ui/editor/caixa_texto_gravar_mapa.png"
    Just caixaSelect <- loadJuicyPNG $ imagensCaminho ++ "ui/editor/caixa_selecionar_mapa.png"

    return [[toolbarTerrenosRelva, toolbarTerrenosEstrada, toolbarTerrenosRio], [toolbarObstaculosArvore, toolbarObstaculosCarro, toolbarObstaculosTronco], [tooltips], [cursor], [caixaInput, caixaSelect]]

alturaMapaEditor :: Int
alturaMapaEditor = 30

larguraMapaEditor :: Int
larguraMapaEditor = 20

data EstadoEditor = EstadoEditor
    {
        chunks :: [Chunk],
        modo :: Int,
        camera :: CameraEditor,
        toolbar :: Int,
        itemSelecionado :: Int,
        objeto :: Coordenadas,
        receberInput :: Bool,
        nomeFicheiro :: String,
        receberSelectMapa :: (Bool, Int),
        mapasGuardados :: [String]
    } deriving Show

estadoEditorInicial :: EstadoEditor
estadoEditorInicial = EstadoEditor (replicate alturaMapaEditor Nada) 0 (0,0,0.75) 0 0 (0, 0) False "" (False, 0) []

desenhaEditor :: EstadoEditor -> [[Picture]] -> [[Picture]] -> Picture
desenhaEditor estado@(EstadoEditor chunks modo (cx,cy, scale) barra item obj _ _ _ _) ps ts = Pictures [Scale scale scale $ Translate (fromIntegral cx) (fromIntegral cy) $ Pictures $ drawChunks (ps !! 0) (ps !! 1) chunks (0,400)++[desenhaCursor obj (head (ts !! 3))], desenhaUIEditor estado ts]

desenhaUIEditor :: EstadoEditor -> [[Picture]] -> Picture
desenhaUIEditor (EstadoEditor chunks modo (cx,cy, scale) barra item _ receber t (select, n) ns) ps = Pictures $ [if barra == 0 then head ps !! item else (ps !! 1) !! item, head $ ps !! 2] ++ if receber then [caixaInput t $ head (ps !! 4) ] else if select then [caixaSelect ns n ((ps !! 4) !! 1)] else []

caixaInput :: String -> Picture -> Picture
caixaInput s p = Pictures [p, Translate (-600) (-70) $ Color white $ Text s]

caixaSelect :: [String] -> Int -> Picture -> Picture
caixaSelect s n ps =  Pictures (ps:formataSelects (map (Text) s) 520 n)

formataSelects :: [Picture] -> Float -> Int -> [Picture]
formataSelects [] _ _ = []
formataSelects (h:t) y i = if i == 0 then (Color white $ Scale 0.5 0.5 $ Translate (-900) y h):formataSelects t (y-150) (i-1) else (Scale 0.5 0.5 $ Color (greyN 0.4) $ Translate (-900) y h):formataSelects t (y-150) (i-1)

desenhaCursor :: Coordenadas -> Picture -> Picture
desenhaCursor (x,y) = Translate (fromIntegral (x * 50 + y * (-50))) (400 - 25 * fromIntegral y - 25 * fromIntegral x)

drawChunks :: [Picture] -> [Picture] -> [Chunk] -> (Float,Float) -> [Picture]
drawChunks _ _ [] _ = []
drawChunks ps psobs (Nada:t) (x,y) = Pictures (drawLinhaNada ps larguraMapaEditor (x,y)):drawChunks ps psobs t (x - 50, y - 25)
drawChunks ps psobs ((Linha linha@(_, obs)):t) (x,y) = Pictures (drawLinha ps larguraMapaEditor linha (x, y) ++ drawLinhaObstaculo psobs obs (x, y) 0 0 False):drawChunks ps psobs t (x - 50, y - 25)

drawLinhaNada :: [Picture] -> Largura -> (Float,Float) -> [Picture]
drawLinhaNada _ 0 _ = []
drawLinhaNada ps lar (x,y) = drawSegmento  (ps !! 3) (x,y):drawLinhaNada ps (lar - 1) (x+50,y-25)

editaChunk :: [Chunk] -> Coordenadas -> Int -> Int -> [Chunk]
editaChunk ch (x,y) tipo item =
    case tipo of
        0 -> case item of
                0 -> take y ch ++ Linha (Relva, replicate (larguraMapaEditor - 1) Nenhum):drop (y+1) ch
                1 -> take y ch ++ Linha (Estrada 1, replicate (larguraMapaEditor - 1) Nenhum):drop (y+1) ch
                2 -> take y ch ++ Linha (Rio 1, replicate (larguraMapaEditor - 1) Nenhum):drop (y+1) ch
        1 -> case ch !! y of
                Linha (Relva, obs) -> if item == 0 then take y ch ++ Linha (Relva, take x obs ++ Arvore:drop (x+1) obs):drop (y+1) ch  else ch
                Linha (Estrada v, obs) -> if item == 1 then take y ch ++ Linha (Estrada v, take x obs ++ Carro:drop (x+1) obs):drop (y+1) ch else ch
                Linha (Rio v, obs) -> if item == 2 then take y ch ++ Linha (Rio v, take x obs ++ Tronco:drop (x+1) obs):drop (y+1) ch else ch
                _ ->  ch

nomeMapasGuardados :: IO [String]
nomeMapasGuardados = do
                        dirAtual <- getCurrentDirectory
                        all <- getDirectoryContents dirAtual
                        let filtered = filter (isSuffixOf ".mapa") all
                        return filtered

leChunks :: String -> IO [Chunk]
leChunks n = do
    ficheiro <- readFile n
    return (read ficheiro)

chunksParaMapa :: [Chunk] -> Mapa
chunksParaMapa chunks = Mapa larguraMapaEditor (map chunkParaLinha chunks)

chunkParaLinha :: Chunk -> (Terreno, [Obstaculo])
chunkParaLinha (Linha l) = l

chunksProntos :: [Chunk] -> Bool
chunksProntos [] = True
chunksProntos (Nada:t) = False
chunksProntos (_:t) = chunksProntos t