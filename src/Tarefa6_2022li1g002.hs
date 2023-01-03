{- |
Module      : Tarefa6_2022li1g002
Description : Implementação do jogo em Gloss
Copyright   : João d'Araújo Dias Lobo <a104356@alunos.uminho.pt>
              Rita da Cunha Camacho <a104439@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
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

{- | O tipo de dados 'Cena' é usado no estado para controlar a lógica da aplicação, o grupo decidiu dividir o jogo em cenas para distinguir menus/modos de jogo.-}
data Cena 
    = MainMenu Int -- ^ Menu principal do jogo.
    | JogoCena (Int,Int) -- ^ Jogo, o tuplo de 'Int' serve para controlar os submenus, primeiro argumento: (@0@ -> Sem Menu, @1@ -> Menu de Pausa, @2@ -> Menu de Game Over), segundo argumento indica a opção do submenu selecionada.
    | Editor  -- ^ Editor de mapas.
    | SkinsMenu -- ^ Menu de escolha de personagem.
    | InfoMenu Int -- ^ Menu de informação sobre o jogo.
    deriving (Eq, Show)

{- | O tipo de dados 'Estado' contém algumas variáveis globais usadas para atualizar o jogo, a função 'tempoReage' utiliza este estado com frequência relativa aos 'fps' do jogo. Este tipo de dados usa "record syntax" que permite ultilizar as suas variáveis pelo nome. -}
data Estado = Estado
    {
        debug :: Bool, -- ^ O 'Bool' debug serve para apresentar alguns dados na tela úteis para fins de desenvolvimento.
        cena :: Cena, -- ^ A 'Cena' em que a aplicação se encontra.
        jogo :: Jogo, -- ^ Quando a aplicação se econtra em 'JogoCena' esta variável com tipo definido em 'LI12223' guarda o estado do jogo estilo Crossy Road.
        movimento :: Jogada, -- ^ Quando o jogador prime uma tecla de movimento (setas), esta variável é alterada para indicar o movimento que o jogador deseja efetuar.
        direcao :: Direcao, -- ^ Esta variável serve para desenhar o jogador na tela tendo em conta a direção para onde "olha", normalmente corresponde à direção do último movimento efetuado.
        vivo :: Bool, -- ^ Indica se o jogo ainda não terminou.
        tick :: Int, -- ^ Para suavizar e animar o jogo independentemente dos 'fps' utilizamos esta variável que é atualizada (+1) atualização do jogo, atualizamos o jogo quando @fps == taxaUpdate@.
        seed :: Int, -- ^ Seed do mapa (valor de 1 a 100 inclusive).
        pontuacao :: (Int,Int), -- ^ Primeiro argumento do tuplo é a pontuação apresentada na tela, o segundo é uma variável de controlo (como o y não cresce à medida que o mapa avança é preciso saber quando é que o jogador realmente "sobe" uma unidade).
        genY :: Int, -- ^ Este valor é usado para gerar a próxima linha do mapa de forma dinâmica.
        editor :: EstadoEditor, -- ^ O 'EstadoEditor' é usado na cena 'Editor'. 
        skin :: Int -- ^ Indica a personagem que está a ser utilizada.
    } deriving Show

{- | Quantidade de linhas que aparecem na tela. -}
alturaMapa :: Int
alturaMapa = 30

{- | Quantidade de colunas que aparecem na tela. -}
larguraMapa :: Int
larguraMapa = 20

{- | Estado inicial do jogo. -}
estadoInicial :: IO Estado
estadoInicial = do
    seed <- randomRIO (1 :: Int, 100 :: Int)
    return $ Estado False (MainMenu 0) (Jogo (Jogador (6,alturaMapa)) (geraMapa larguraMapa alturaMapa seed)) Parado Cima True 0 seed (0,0) (alturaMapa + 4) estadoEditorInicial 0

{- | Define o display da aplicação Gloss. -}
dm :: Display
dm = InWindow "Monad Road" dimensoesDisplay (0,0)

{- | Dimensões da janela do jogo @(1600, 900)@. -}
dimensoesDisplay :: (Int, Int)
dimensoesDisplay = (1600, 900)

{- | Caminho para os assets gráficos. -}
imagensCaminho :: String
imagensCaminho = "assets/images/"

{- | __Frames per second__ , número de vezes que o jogo é desenhado num segundo. -}
fps :: Int
fps = 30

{- | Número de ticks a que o jogo tem de chegar para voltar a ser atualizado. -}
taxaUpdate :: Int
taxaUpdate = 50

{- | A função 'drawEstado' controla o que deve ser desenhado dependendo do estado da aplicação. -}
drawEstado :: [[Picture]] -- ^ Lista de imagens previamente carregadas.
    -> Estado -- ^ 'Estado' da aplicação.
    -> IO Picture -- ^ Retorna a 'Picture' que neste caso é o frame completo que a aplicação desenhou.
drawEstado ps (Estado _ (MainMenu m) _ _ _ _ _ _ _ _ _ _) = return $ drawMenu (MainMenu m) $ head ps
drawEstado ps (Estado debug (JogoCena jn) j@(Jogo p@(Jogador c@(x,y)) m) _ d vivo t s (pt,_) gy _ sn)
    | debug = return $ Pictures [drawJogo [ps !! 1,ps !! 2,ps !! 3] m c d t (pt >= 4 && vivo) sn, drawMuros,drawDebugUI s (x,y) t d (not vivo) gy, drawJogoUI pt jn (ps !! 4) (ps !! 5) (ps !! 7)]
    | otherwise = return $ Pictures [drawJogo [ps !! 1,ps !! 2, ps !! 3] m c d t (pt >= 4 && vivo) sn, drawMuros, drawJogoUI pt jn (ps !! 4) (ps !! 5) (ps !! 7)]
drawEstado ps (Estado _ Editor _ _ _ _ _ _ _ _ estadoEditor _) = do
    imagensEditor <- editorDeMapasImagens
    return $ drawEditor estadoEditor [(ps !! 1),(ps !! 2)] imagensEditor
drawEstado ps (Estado _ SkinsMenu _ _ _ _ _ _ _ _ _ nSkin) = return $ drawSkinsMenu (head (ps !! 6)) (ps !! 3) nSkin
drawEstado ps (Estado _ (InfoMenu 2) _ _ _ _ _ _ _ _ _ _) = do
    recorde <- atualizaRecorde 0
    return $ Pictures [(ps !! 8) !! 2, Translate 170 0 $ drawPontuacao recorde (ps !! 5)]
drawEstado ps (Estado _ (InfoMenu n) _ _ _ _ _ _ _ _ _ _) = return $ (ps !! 8) !! n

{- | A função 'drawSkinsMenu' desenha o menu de escolha de personagem. -}
drawSkinsMenu :: Picture -- ^ Imagem correspondente ao fundo do menu. 
    -> [Picture] -- ^ Imagens de todas as personagens.
    -> Int -- ^ Personagem selecionada.  
    -> Picture -- ^ Retorna a imagem gerada juntando os elementos que recebe.
drawSkinsMenu fundo ps n = Pictures [fundo, Scale 3 3 $ Translate 0 (-15) $ ps !! (1 + n*4)]

{- | A função 'drawMenu' desenha o menu principal. -}
drawMenu :: Cena -- ^ A variável do tipo 'Cena' contém a opção selecionada.
    -> [Picture] -- ^ Imagens que formam o menu.
    -> Picture -- ^ Retorna a imagem relativa à opção selecionada.
drawMenu menu ps =
    case menu of
        MainMenu n -> ps !! n

{- | A função 'drawJogoUI' desenha a interface do jogo, pontuação, menu de pausa, tela de game over. -}
drawJogoUI :: Int -- ^ Pontuação atual.
    -> (Int, Int) -- ^ Submenu aberto e a sua opção selecionada.
    -> [Picture] -- ^ Imagens do menu de pausa.
    -> [Picture] -- ^ Imagens dos números que formam o mostrador da pontuação.
    -> [Picture] -- ^ Imagens da tela de game over.
    -> Picture -- ^ Imagem gerada.
drawJogoUI p (0, _) _ ns _ = Translate 0 360 $ Scale 0.5 0.5 $ drawPontuacao p ns
drawJogoUI _ (1, op) ps _ _ = ps !! op
drawJogoUI _ (2, op) _ _ ps = ps !! op

{- | A função 'drawPontuacao' é auxilar de 'drawJogoUI', desenha um @n@ do tipo 'Int' desde que @n >= 0 && n < 1000@ através de imagens com os números de 0 - 9 . -}
drawPontuacao :: Int -- ^ Número a ser desenhado.
    -> [Picture] -- ^ Imagens dos números.
    -> Picture -- ^ Número em imagem.
drawPontuacao pt ps
 | pt < 10 = ps !! pt
 | pt < 100 = Translate (-25) 0 $ Pictures [ps !! div pt 10, Translate 68 0 $ ps !! mod pt 10]
 | otherwise = Translate (-50) 0 $ Pictures [ps !! div (div pt 10) 10, Translate 68 0 $ ps !! mod (div pt 10) 10, Translate 136 0 $ ps !! mod pt 10]

{- | A função 'drawDebugUI' desenha algumas estatísticas para fins de desenvolvimento e debugging no canto superior esquerdo da aplicação. -}
drawDebugUI :: Int -- ^ Seed do mapa atual.
     -> (Int,Int) -- ^ Posição do jogador.
     -> Int -- ^ Número de ticks desde a última atualização do mapa.
     -> Direcao -- ^ Direção atual do jogador.
     -> Bool -- ^ Direção atual do jogador.
     -> Int -- ^ 'Bool' que indica se o jogo terminou.
     -> Picture -- ^ Retorna um painel de estatísticas.
drawDebugUI s (x,y) t d m gy = Pictures [Translate (-785) 400 $ Scale 0.2 0.2 (Text ("SEED: "++show s)), Translate (-785) 350 $ Scale 0.2 0.2 (Text ("POSICAO: "++show x++", "++show y)), Translate (-785) 300 $ Scale 0.2 0.2 (Text ("DIRECAO: "++show d)), Translate (-785) 250 $ Scale 0.2 0.2 (Text ("MORTO?: "++show m)), Translate (-785) 200 $ Scale 0.2 0.2 (Text ("TICK: "++show t))]

{- | A função 'drawMuros' desenha umas barreiras à volta do mapa para esconder alguns obstáculos que ao serem animados passam pelas bordas do mapa. -}
drawMuros :: Picture
drawMuros = Pictures [Translate 0 370 $ Rotate 63 $ Color corFundo $ rectangleSolid 150 2000, Translate (-400) 300 $ Rotate 64 $ Color corFundo $ rectangleSolid 150 400]

{- | A função 'drawJogo' desenha o jogo principal da aplicação, utiliza as funções auxiliares 'drawJogador', 'drawMapa' e 'drawObstaculos'. É também encarregue do deslize suave do mapa. -}
drawJogo :: [[Picture]] -- ^ Lista de imagens do jogo. 
    -> Mapa -- ^ Mapa a ser desenhado.
    -> Coordenadas -- ^ Coordenadas da posição atual do jogador.
    -> Direcao -- ^ Direção atual do jogador.
    -> Int -- ^ Ticks desde a última atualização do jogo, usado para animar o os obstáculos suavemente (translação suave).
    -> Bool -- ^ Indica se o mapa já deve deslizar.
    -> Int -- ^ Personagem a ser utilizada pelo jogador.
    -> Picture -- ^ Retorna a imagem do jogo.
drawJogo ps m@(Mapa l ls) co@(x,y) d t desliza sn = Translate (fromIntegral t * dx) (fromIntegral t * dy) $ Translate (fromIntegral alturaMapa * 50 - 900) (fromIntegral alturaMapa * 25 - 600) $ Pictures $ drawMapa (head ps) m (0,400)++drawObstaculos (ps !! 1) m (0,400) t co++[drawJogador (ps !! 2) d co v t sn]
    where v =
            let linha = ls !! y in
            case fst linha of
                Rio vv -> if x >= 0 && x <= l && snd linha !! x == Tronco then vv else 0
                _ -> 0
          (dx, dy) = if desliza then (-1, -0.5) else (0, 0)

{- | A função 'drawJogador' desenha o jogador na tela. -}
drawJogador :: [Picture] -- ^ Lista de imagens do jogador (inclui todas as personagens).
    -> Direcao -- ^ Direção atual do jogador.
    -> Coordenadas  -- ^ Coordenadas da posição atual do jogador.
    -> Velocidade -- ^ Velocidade da linha onde o jogador se encontra (usado para animar o jogador suavemente quando este está em cima de um tronco).
    -> Int -- ^ Ticks desde a última atualização do jogo.
    -> Int -- ^ Personagem a ser utilizada.
    -> Picture -- ^ Retorna a imagem do jogador.
drawJogador ps d (x,y) 0 _ sn =
    case d of
        Cima -> Translate  (fromIntegral (x * 50 + y * (-50))) (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! (0 + 4*sn))
        Baixo -> Translate (fromIntegral (x * 50 + y * (-50)))  (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! (1 + 4*sn))
        Esquerda -> Translate (fromIntegral (x * 50 + y * (-50)))  (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! (2 + 4*sn))
        Direita -> Translate (fromIntegral (x * 50 + y * (-50))) (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! (3 + 4*sn))
drawJogador ps d (x,y) v t sn =
    case d of
        Cima -> Translate (fromIntegral $ t * v) (fromIntegral (t * v) * (- 0.5)) $ Translate  (fromIntegral (x * 50 + y * (-50))) (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! (0 + 4*sn))
        Baixo -> Translate (fromIntegral $ t * v) (fromIntegral (t * v) * (- 0.5)) $ Translate (fromIntegral (x * 50 + y * (-50)))  (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! (1 + 4*sn))
        Esquerda -> Translate (fromIntegral $ t * v) (fromIntegral (t * v) * (- 0.5)) $ Translate (fromIntegral (x * 50 + y * (-50)))  (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! (2 + 4*sn))
        Direita -> Translate (fromIntegral $ t * v) (fromIntegral (t * v) * (- 0.5)) $ Translate (fromIntegral (x * 50 + y * (-50))) (400 - 25 * fromIntegral y - 25 * fromIntegral x) (ps !! (3 + 4*sn))

{- | A função __recursiva__ 'drawMapa' desenha a base das linhas do jogo (terrenos) tendo em conta que o jogo é isométrico. -}
drawMapa :: [Picture] -- ^ Imagens dos terrenos.
    -> Mapa -- ^ Mapa a ser desenhado.
    -> (Float,Float) -- ^ (x,y) variam a cada iteração da função para desenhar o mapa de forma isométrica.
    -> [Picture] -- ^ Lista de imagens geradas (uma para cada linha do mapa).
drawMapa _ (Mapa la []) _ = []
drawMapa ps m@(Mapa la ls) (x,y) = Pictures (drawLinha ps la (head ls) (x, y)): drawMapa ps (Mapa la $ tail ls) (x - 50, y - 25)

{- | A função __recursiva__ 'drawLinha' é auxiliar de 'drawMapa' desenha uma linha na tela. -}
drawLinha :: [Picture] -- ^ Imagens dos terrenos.
    -> Largura -- ^ Número de colunas a serem desenhadas. 
    -> (Terreno, [Obstaculo])  -- ^ Linha a ser desenhada. 
    -> (Float,Float) -- ^ (x,y) variam a cada iteração da função para desenhar a linha de forma isométrica.
    -> [Picture] -- ^ Linha em imagem.
drawLinha _ 0 _ _ = []
drawLinha ps lar (t, obs) (x,y) =
    case t of
        Relva -> drawSegmento (head ps) (x,y):drawLinha ps (lar - 1) (t,obs) (x+50,y-25)
        Rio _ -> drawSegmento (ps !! 1) (x,y):drawLinha ps (lar - 1) (t,obs) (x+50,y-25)
        Estrada _ -> drawSegmento (ps !! 2) (x,y):drawLinha ps (lar - 1) (t,obs) (x+50,y-25)

{- | A função 'drawSegmento' desenha uma imagem na grelha do jogo. -}
drawSegmento :: Picture -- ^ Asset da imagem a ser desenhada.
    -> (Float,Float) -- ^ Posição da imagem na tela.
    -> Picture -- ^ Imagem desenhada.
drawSegmento p (x,y) = Translate x y p

{- | A função 'drawObstaculos' desenha os obstáculos do jogo. -}
drawObstaculos :: [Picture]  -- ^ Assets das imagens dos obstáculos.
    -> Mapa -- ^ Mapa a ser desenhado.
    -> (Float,Float) -- ^ (x,y) variam a cada iteração da função para desenhar a linha de forma isométrica.
    -> Int -- ^ Ticks desde a última atualização do jogo.
    -> Coordenadas -- ^ Posição do jogador no mapa, permite parar a animação dos carros no caso de atropelamento.
    -> [Picture] -- ^ Lista de imagens geradas (uma para cada linha de obstáculos do mapa).
drawObstaculos _ (Mapa la []) _ _ _ = []
drawObstaculos ps m@(Mapa la ls) (x,y) t cordsJogador@(xx,yy) = let deveEstarParado = yy == 0 && (snd (head ls) !! xx) == Carro in
    Pictures (drawLinhaObstaculo ps (snd (head ls)) (x, y) v t deveEstarParado): drawObstaculos ps (Mapa la $ tail ls) (x - 50, y - 25) t (xx, yy-1)
    where v = case fst (head ls) of
                Rio vv -> vv
                Estrada vv -> vv
                Relva -> 0

{- |  A função 'drawLinhaObstaculo' uma linha de obstáculos. -}
drawLinhaObstaculo :: [Picture] -- ^ Assets das imagens dos obstáculos.
    -> [Obstaculo] -- ^ Lista de obstáculos a serem desenhados.
    -> (Float,Float) -- ^ (x,y) variam a cada iteração da função para desenhar a linha de obstáculos de forma isométrica.
    -> Velocidade -- ^ Velocidade da linha de obstáculos a ser desenhada.
    -> Int -- ^ Ticks desde a última atualização do jogo.
    -> Bool -- ^ Caso a linha tenha causado um atropelamento, este 'Bool' indica se a animação dos carros deve parar.
    -> [Picture] -- ^ Imagem da linha de obstáculos gerada.
drawLinhaObstaculo _ [] _ _ _ _ = []
drawLinhaObstaculo ps obs@(h:t) (x,y) v ti p =
    case h of
        Arvore -> drawSegmento (head ps) (x,y+10):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p
        Tronco ->  Translate (fromIntegral $ ti * v) (fromIntegral (ti * v) * (- 0.5)) (drawSegmento (ps !! 1) (x,y)):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p
        Carro -> if v > 0 then if not p then Translate (fromIntegral $ ti * v) (fromIntegral (ti * v) * (- 0.5)) (drawSegmento (ps !! 2) (x,y)):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p else (drawSegmento (ps !! 2) (x,y)):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p
        else if v < 0 then if not p then Translate (fromIntegral $ ti * v) (fromIntegral (ti * v) * (- 0.5)) (drawSegmento (ps !! 3) (x,y)):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p else drawSegmento (ps !! 3) (x,y):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p
        else drawSegmento (ps !! 2) (x,y):drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p
        Nenhum -> drawLinhaObstaculo ps (tail obs) (x+50,y-25) v ti p

{- | A função 'inputReage' controla todas as operações que devem ser feitas quando a aplicação recebe um input do jogador. -}
inputReage :: Event -- ^ Nesta aplicação só usamos eventos do tipo 'EventKey', numa futura autalização seria possível utilizar 'EventMotion' para receber a posição do cursor e potencialmente permitir o uso do rato em menus.
    -> Estado -- ^ Estado da aplicação.
    -> IO Estado -- ^ Novo estado (pós input).

-- INPUT DEBUG
inputReage (EventKey (Char 'l') Down _ _) estado@(Estado d _ _ _ _ _ _ _ _ _ _ _) = return estado{ debug = not d }

-- INPUT MAIN MENU
inputReage (EventKey (SpecialKey KeyRight) Down _ _) estado@(Estado _ (MainMenu n) _ _ _ _ _ _ _ _ _ _) = return estado{ cena = MainMenu (proximoN n 3 1) }
inputReage (EventKey (SpecialKey KeyLeft) Down _ _) estado@(Estado _ (MainMenu n) _ _ _ _ _ _ _ _ _ _) = return estado{ cena = MainMenu (proximoN n 3 (-1)) }
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ (MainMenu 3) _ _ _ _ _ _ _ _ _ _) = do
    putStr "Jogo encerrado."
    exitSuccess
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ (MainMenu 1) _ _ _ _ _ _ _ _ _ _) = return estado{ cena = Editor }
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ (MainMenu 0) _ _ _ _ _ _ _ _ _ _) = return estado{ cena = SkinsMenu }
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ (MainMenu 2) _ _ _ _ _ _ _ _ _ _) = return estado{ cena = InfoMenu 0 }

-- INPUT MENU SKINS
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ SkinsMenu _ _ _ _ _ _ _ _ _ _) = return estado{ cena = JogoCena (0, 0) }
inputReage (EventKey (SpecialKey KeyRight) Down _ _) estado@(Estado _ SkinsMenu _ _ _ _ _ _ _ _ _ n) = return estado{ skin = proximoN n 3 1 }
inputReage (EventKey (SpecialKey KeyLeft) Down _ _) estado@(Estado _ SkinsMenu _ _ _ _ _ _ _ _ _ n) = return estado{ skin = proximoN n 3 (-1) }
inputReage (EventKey (SpecialKey KeyEsc) Down _ _) estado@(Estado _ SkinsMenu _ _ _ _ _ _ _ _ _ _) = return estado{ cena = MainMenu 0, skin = 0 }

--INPUT MENU INFO
inputReage (EventKey (SpecialKey KeyUp) Down _ _) estado@(Estado _ (InfoMenu n) _ _ _ _ _ _ _ _ _ _) = return estado{ cena = InfoMenu $ proximoN n 3 (-1) }
inputReage (EventKey (SpecialKey KeyDown) Down _ _) estado@(Estado _ (InfoMenu n) _ _ _ _ _ _ _ _ _ _) = return estado{ cena = InfoMenu $ proximoN n 3 1 }
inputReage (EventKey (SpecialKey KeyEsc) Down _ _) estado@(Estado _ (InfoMenu n) _ _ _ _ _ _ _ _ _ _) = return estado{ cena = MainMenu 0 }

-- INPUT JOGO PRINCIPAL
inputReage (EventKey (Char 'g') Down _ _) estado@(Estado _ (JogoCena (0,_)) _ _ _ _ _ s _ _ _ _) = return estado{ jogo = Jogo (Jogador (0,alturaMapa)) (geraMapa larguraMapa alturaMapa (s+1)), seed = 1+s, tick = 0, vivo = True }
inputReage (EventKey (SpecialKey KeyLeft)  Down _ _) estado@(Estado _ (JogoCena (0,_)) _ _ _ _ t _ _ _ _ _) = return estado{ movimento = Move Esquerda, direcao = Esquerda }
inputReage (EventKey (SpecialKey KeyUp) Down _ _) estado@(Estado _ (JogoCena (0,_)) _ _ _ _ t _ _ _ _ _) = return estado{ movimento = Move Cima, direcao = Cima }
inputReage (EventKey (SpecialKey KeyDown) Down _ _) estado@(Estado _ (JogoCena (0,_)) _ _ _ _ t _ _ _ _ _) = return estado{ movimento = Move Baixo, direcao = Baixo }
inputReage (EventKey (SpecialKey KeyRight)  Down _ _) estado@(Estado _ (JogoCena (0,_)) _ _ _ _ t _ _ _ _ _) = return estado{ movimento = Move Direita, direcao = Direita }
inputReage (EventKey (SpecialKey KeyEsc)  Down _ _) estado@(Estado _ (JogoCena (jn,_)) _ _ _ _ t _ _ _ _ _) = return estado{ cena = JogoCena (abs (jn - 1),0) }

-- INPUT MENU DE PAUSA
inputReage (EventKey (SpecialKey KeyUp) Down _ _) estado@(Estado _ (JogoCena (1,n)) _ _ _ _ t _ _ _ _ _) = return estado{ cena = JogoCena (1 ,proximoN n 1 (-1)) }
inputReage (EventKey (SpecialKey KeyDown) Down _ _) estado@(Estado _ (JogoCena (1,n)) _ _ _ _ t _ _ _ _ _) = return estado{ cena = JogoCena (1 ,proximoN n 1 1) }
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ (JogoCena (1,0)) _ _ _ _ t _ _ _ _ _) = return estado{ cena = JogoCena (0 , 0) }
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ (JogoCena (1,1)) _ _ _ _ t s _ _ _ _) = do estadoInicial;

-- INPUT GAME OVER
inputReage (EventKey (SpecialKey KeyUp) Down _ _) estado@(Estado _ (JogoCena (2,n)) _ _ _ _ t _ _ _ _ _) = return estado{ cena = JogoCena (2 ,proximoN n 1 (-1)) }
inputReage (EventKey (SpecialKey KeyDown) Down _ _) estado@(Estado _ (JogoCena (2,n)) _ _ _ _ t _ _ _ _ _) = return estado{ cena = JogoCena (2 ,proximoN n 1 1) }
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ (JogoCena (2,0)) _ _ _ _ t _ _ _ _ _) = do novo <- estadoInicial; return novo{ cena = JogoCena (0 , 0)}
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ (JogoCena (2,1)) _ _ _ _ t _ _ _ _ _) = do estadoInicial;

-- INPUT EDITOR DE MAPAS
inputReage (EventKey (Char 'w') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ (x,y) False _ (False, _) _) _) = return estado{ editor = editor{objeto = (x, proximoN y (alturaMapaEditor - 1) (-1)) }}
inputReage (EventKey (Char 's') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ (x,y) False _ (False, _) _) _) = return estado{ editor = editor{objeto = (x, proximoN y (alturaMapaEditor - 1) 1) }}
inputReage (EventKey (Char 'd') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ (x,y) False _ (False, _) _) _) = return estado{ editor = editor{objeto = (proximoN x (larguraMapaEditor - 1) 1, y) }}
inputReage (EventKey (Char 'a') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ (x,y) False _ (False, _) _) _) = return estado{ editor = editor{objeto = (proximoN x (larguraMapaEditor - 1) (-1), y) }}
inputReage (EventKey (SpecialKey KeySpace)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor chunks _ _ tool item coord False _ (False, _) _) _) = return estado{ editor = editor{ chunks = editaChunk chunks coord tool item } }
inputReage (EventKey (SpecialKey KeyEsc)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ False _ (False, _) _) _) = return estado{ editor = estadoEditorInicial, cena = MainMenu 0 }
inputReage (EventKey (SpecialKey KeyEsc)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ True _ _ _) _) = return estado{ editor = editor{ receberInput = False, nomeFicheiro = ""} }
inputReage (EventKey (SpecialKey KeyEsc)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ False _ (True, _) _) _) = return estado{ editor = editor{ receberSelectMapa = (False, 0), mapasGuardados = []} }
inputReage (EventKey (Char '+')  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ (cx,cy,scale) _ _ _ False _ (False, _) _) _) = return estado{ editor = editor{ camera = (cx, cy, scale + scale / 2)} }
inputReage (EventKey (Char '-')  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ (cx,cy,scale) _ _ _ False _ (False, _) _) _) = return estado{ editor = editor{ camera = (cx, cy, scale - scale / 2)} }
inputReage (EventKey (SpecialKey KeyRight)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ n _ _ False _ (False, _) _) _) = return estado{ editor = editor{ toolbar = proximoN n 1 1, itemSelecionado = 0 } }
inputReage (EventKey (SpecialKey KeyLeft) Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ n _ _ False _ (False, _) _) _) = return estado{ editor = editor{ toolbar = proximoN n 1 1, itemSelecionado = 0} }
inputReage (EventKey (SpecialKey KeyUp)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ n _ False _ (False, _) _) _) = return estado{ editor = editor{ itemSelecionado = proximoN n 2 (-1) } }
inputReage (EventKey (SpecialKey KeyDown) Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ n _ False _ (False, _) _) _) = return estado{ editor = editor{ itemSelecionado = proximoN n 2 1} }
inputReage (EventKey (Char c) Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ True t (False, _) _) _) = return estado{ editor = editor{ nomeFicheiro = t++[c]}}
inputReage (EventKey (SpecialKey KeyBackspace) Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ True t (False, _) _) _) = return estado{ editor = editor{ nomeFicheiro = drop 1 t}}
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ True t (False, _) _) _) = return estado{ editor = editor{ receberInput = False }}
inputReage (EventKey (Char 'g') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor chunks _ _ _ _ _ False _ (False, _) _) _) = return estado{ editor = editor{ receberInput = True }}
inputReage (EventKey (Char 'c') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor chunks _ _ _ _ _ False _ (False, _) _) _) =
    do
        nomeMapas <- nomeMapasGuardados
        return estado{ editor = editor{ receberSelectMapa = (True, 0), mapasGuardados = nomeMapas}}
inputReage (EventKey (SpecialKey KeyEnter) Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ _ _ False _ (True, n) _) _) = return estado{ editor = editor{ receberSelectMapa = (False, n) }}
inputReage (EventKey (SpecialKey KeyUp)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ n _ False _ (True, index) ns) _) = return estado{ editor = editor{ receberSelectMapa = (True, proximoN index (length ns - 1) (-1)) } }
inputReage (EventKey (SpecialKey KeyDown)  Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor _ _ _ _ n _ False _ (True, index) ns) _) = return estado{ editor = editor{ receberSelectMapa = (True, proximoN index (length ns - 1) (1)) } }
inputReage (EventKey (Char 'p') Down _ _) estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor chunks _ _ _ _ _ False _ (False, _) _) _) = if chunksProntos chunks then return estado{ jogo = Jogo (Jogador (6,alturaMapaEditor-1)) (chunksParaMapa chunks), cena = (JogoCena (0,0)), editor = estadoEditorInicial } else return estado

inputReage _ e = return e

{- | A função 'proximoN' é utilizada para decidir qual a próxima opção a ser selecionada quando o jogador clica nas setas num menu, evita bastante repetição de código. -}
proximoN :: Int -- ^ Opção do menu antes de receber o input.
    -> Int -- ^ Número da última opção.
    -> Int -- ^ (+1 ou -1) dependendo da seta em que o jogador carregou.
    -> Int -- ^ Retorna a próxima opção.
proximoN c max i
  | c + i > max = 0
  | c + i < 0 = max
  | otherwise = c + i

{- | A função 'tempoReage' é chamada sempre que um frame é desenhado, dependendo da cena, dos ticks e de outros argumentos do estado, a função decide que operação deve efetuar (se for necessário).  -}
tempoReage :: Float -- ^ Período de tempo em segundos que o jogo precisa de avançar (a nossa implementação não utiliza este argumento).
    -> Estado -- ^ Estado antes da atualização.
    -> IO Estado -- ^ Novo estado.
tempoReage f estado@(Estado _ (JogoCena (0,_)) jogo@(Jogo j@(Jogador (x,y)) m@(Mapa _ ls)) movimento _ vivo t seed (pt,auxPt) gy _ _)
    | t /= taxaUpdate && mod t 10 == 0 = if jogoTerminou jogo then do
        atualizaRecorde pt
        return estado{ cena = JogoCena (2,0) } else return estado{tick = t+1 }
    | t == taxaUpdate && vivo = let novoJogo = animaJogo jogo Parado; desliza = pt >= 4 in return estado{ jogo = if not desliza then novoJogo else deslizaJogo (randoms (mkStdGen seed) !! gy)  novoJogo, tick = 0, vivo = not $ jogoTerminou novoJogo, genY = if desliza then gy + 1 else gy}
    | t == taxaUpdate = return estado{tick = 0 }
    | movimento /= Parado = let jogador@(Jogador (xx,yy)) = moveJogador j movimento $ arranjaRios ls; novoJogo = Jogo jogador m; novoAuxPt = if yy < y then auxPt + 1 else if yy > y then auxPt - 1 else auxPt; in
        return estado{ jogo = novoJogo, movimento = Parado, tick = t+1, vivo = not $ jogoTerminou novoJogo, pontuacao = if novoAuxPt > 0 then (pt + 1,0) else (pt,novoAuxPt)}
    | otherwise = return estado{ tick = t+1 }
tempoReage _ estado@(Estado _ Editor _ _ _ _ _ _ _ _ editor@(EstadoEditor chunks _ _ _ _ _ False t (False, s) ns) _)
    | t /= "" = do
        writeFile (t++".mapa") (show chunks)
        putStr "O mapa que estava a ser editado foi gravado"
        return estado{ editor = editor{ receberInput = False, nomeFicheiro = "" }}
    | ns /= [] = do
        novosChunks <- leChunks (ns !! s)
        return estado{ editor = editor{ receberSelectMapa = (False,0), mapasGuardados = [], chunks = novosChunks }}
    | otherwise = return estado
tempoReage _ estado = return estado

{- | Cor do fundo da aplicação. (RGBA de 0 a 1) -}
corFundo :: Color -- ^ Cor.
corFundo = makeColor (79/255) (112/255) (126/255) 1

{- | A função 'main' carrega todas as imagens para a memória da aplicação e chama a função 'playIO' do Gloss. 
Para carregar imagens do tipo .png (ao contrário de .bmp), decidimos utlizar uma biblioteca externa 'Graphics.Gloss.Juicy'.
-}
jogoLoop :: IO()
jogoLoop = do
    -- Main Menu
    Just mm00 <- loadJuicyPNG $ imagensCaminho ++ "ui/mainmenu/mainmenu00.png"
    Just mm01 <- loadJuicyPNG $ imagensCaminho ++ "ui/mainmenu/mainmenu01.png"
    Just mm02 <- loadJuicyPNG $ imagensCaminho ++ "ui/mainmenu/mainmenu02.png"
    Just mm03 <- loadJuicyPNG $ imagensCaminho ++ "ui/mainmenu/mainmenu03.png"
    -- Skins Menu
    Just sm <- loadJuicyPNG $ imagensCaminho ++ "ui/skinsmenu/skinsmenu.png"
    -- Info Menu
    Just in00 <- loadJuicyPNG $ imagensCaminho ++ "ui/infomenu/infomenu00.png"
    Just in01 <- loadJuicyPNG $ imagensCaminho ++ "ui/infomenu/infomenu01.png"
    Just in02 <- loadJuicyPNG $ imagensCaminho ++ "ui/infomenu/infomenu02.png"
    Just in03 <- loadJuicyPNG $ imagensCaminho ++ "ui/infomenu/infomenu03.png"
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
    Just galinhaCima <- loadJuicyPNG $ imagensCaminho ++ "jogador/galinha/galinhaCima.png"
    Just galinhaBaixo <- loadJuicyPNG $ imagensCaminho ++ "jogador/galinha/galinhaBaixo.png"
    Just galinhaEsquerda <- loadJuicyPNG $ imagensCaminho ++ "jogador/galinha/galinhaEsquerda.png"
    Just galinhaDireita <- loadJuicyPNG $ imagensCaminho ++ "jogador/galinha/galinhaDireita.png"
    Just caoCima <- loadJuicyPNG $ imagensCaminho ++ "jogador/cao/caoCima.png"
    Just caoBaixo <- loadJuicyPNG $ imagensCaminho ++ "jogador/cao/caoBaixo.png"
    Just caoEsquerda <- loadJuicyPNG $ imagensCaminho ++ "jogador/cao/caoEsquerda.png"
    Just caoDireita <- loadJuicyPNG $ imagensCaminho ++ "jogador/cao/caoDireita.png"
    Just ninjaCima <- loadJuicyPNG $ imagensCaminho ++ "jogador/ninja/ninjaCima.png"
    Just ninjaBaixo <- loadJuicyPNG $ imagensCaminho ++ "jogador/ninja/ninjaBaixo.png"
    Just ninjaEsquerda <- loadJuicyPNG $ imagensCaminho ++ "jogador/ninja/ninjaEsquerda.png"
    Just ninjaDireita <- loadJuicyPNG $ imagensCaminho ++ "jogador/ninja/ninjaDireita.png"
    Just xadrezCima <- loadJuicyPNG $ imagensCaminho ++ "jogador/xadrez/xadrezCima.png"
    Just xadrezBaixo <- loadJuicyPNG $ imagensCaminho ++ "jogador/xadrez/xadrezBaixo.png"
    Just xadrezEsquerda <- loadJuicyPNG $ imagensCaminho ++ "jogador/xadrez/xadrezEsquerda.png"
    Just xadrezDireita <- loadJuicyPNG $ imagensCaminho ++ "jogador/xadrez/xadrezDireita.png"
    -- Menu Pausa
    Just mp00 <- loadJuicyPNG $ imagensCaminho ++ "ui/pausa/pausa00.png"
    Just mp01 <- loadJuicyPNG $ imagensCaminho ++ "ui/pausa/pausa01.png"
    -- Tela Game Over
    Just go00 <- loadJuicyPNG $ imagensCaminho ++ "ui/gameover/gameover00.png"
    Just go01 <- loadJuicyPNG $ imagensCaminho ++ "ui/gameover/gameover01.png"
    -- Números pontuação
    Just p0 <- loadJuicyPNG $ imagensCaminho ++ "ui/numeros/0.png"
    Just p1 <- loadJuicyPNG $ imagensCaminho ++ "ui/numeros/1.png"
    Just p2 <- loadJuicyPNG $ imagensCaminho ++ "ui/numeros/2.png"
    Just p3 <- loadJuicyPNG $ imagensCaminho ++ "ui/numeros/3.png"
    Just p4 <- loadJuicyPNG $ imagensCaminho ++ "ui/numeros/4.png"
    Just p5 <- loadJuicyPNG $ imagensCaminho ++ "ui/numeros/5.png"
    Just p6 <- loadJuicyPNG $ imagensCaminho ++ "ui/numeros/6.png"
    Just p7 <- loadJuicyPNG $ imagensCaminho ++ "ui/numeros/7.png"
    Just p8 <- loadJuicyPNG $ imagensCaminho ++ "ui/numeros/8.png"
    Just p9 <- loadJuicyPNG $ imagensCaminho ++ "ui/numeros/9.png"

    estado <- estadoInicial

    playIO dm
        corFundo
        fps
        estado
        (drawEstado [[mm00, mm01, mm02, mm03], [relva,rio,estrada,nenhum], [arvore, tronco, carroDireita, carroEsquerda], [galinhaCima, galinhaBaixo, galinhaEsquerda, galinhaDireita, caoCima, caoBaixo, caoEsquerda, caoDireita, ninjaCima, ninjaBaixo, ninjaEsquerda, ninjaDireita, xadrezCima, xadrezBaixo, xadrezEsquerda, xadrezDireita], [mp00, mp01], [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9], [sm], [go00, go01], [in00, in01, in02, in03]])
        inputReage
        tempoReage

{- | A função 'atualizaRecorde' permite guardar a pontuação mais elevada do utilizador até ao momento num ficheiro highscore.save. -}
atualizaRecorde :: Int -- ^Nova pontuação obtida.
    -> IO Int -- ^Melhor pontuação.
atualizaRecorde n = do
    ficheiroExiste <- doesFileExist "highscore.save"
    ficheiro <- if ficheiroExiste then readFile "highscore.save"
                else return "0"
    let bestAntigo = read ficheiro :: Int
    bestReal <- if n > bestAntigo then return n else return bestAntigo
    writeFile "highscore.save" (show bestReal)
    return bestReal

{- | A função __não recursiva__ 'geraMapa', através da função auxiliar 'geraMapaAux' e da função 'obterRandoms' presente no módulo da tarefa 2, gera um mapa pseudo-aleatório utilizando uma /seed/ e uma largura.-}
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
arranjaRios :: [(Terreno, [Obstaculo])] -- ^ Linhas do mapa a serem corrigidas.
    -> [(Terreno, [Obstaculo])] -- ^ Linhas pós correção dos rios.
arranjaRios [] = []
arranjaRios ((t,obs):tt) =
    case t of
        Rio v -> (Rio 0, obs):arranjaRios tt
        _ -> (t,obs):arranjaRios tt

-- =============== EDITOR DE MAPAS ===============

{- | Para o editor de mapas decidimos usar um tipo de dados diferente para a representação de linhas. 'Chunk' pode ser 'Nada' (sem terreno definido) ou '(Terreno, [Obstaculo])' -}
data Chunk = Linha (Terreno, [Obstaculo]) | Nada deriving (Show, Read)

{- | Estado da câmera do editor @(x, y, zoom)@ -}
type CameraEditor = (Int, Int, Float)

{- | De forma a evitar o carregamento de imagens a mais no início da aplicação, o carregamento das imagens do editor faz-se apenas quando necessário. -}
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

{- | Quantidade de linhas que aparecem na tela do editor. -}
alturaMapaEditor :: Int
alturaMapaEditor = 30

{- | Quantidade de colunas que aparecem na tela editor. -}
larguraMapaEditor :: Int
larguraMapaEditor = 20

{- | O 'EstadoEditor' controla os menus e interações do editor de mapas (faz parte do estado geral da aplicação mas só é alterado quando o utilizador está no editor de mapas). -}
data EstadoEditor = EstadoEditor
    {
        chunks :: [Chunk], -- ^ Lista de chunks do mapa a ser editado.
        modo :: Int, -- ^ Modo de edição.
        camera :: CameraEditor, -- ^ Definições da câmera.
        toolbar :: Int, -- ^ Toolbar a ser utilizada (toolbar de terrenos, toolbar de obstáculos).
        itemSelecionado :: Int, -- ^ Item da toolbar a ser utilizado.
        objeto :: Coordenadas, -- ^ Coordenadas do cursor do utilizador.
        receberInput :: Bool, -- ^ Indica se a aplicação está a receber inputs numa caixa de texto.
        nomeFicheiro :: String, -- ^ Nome do ficheiro (apenas utilizado quando o jogador está a gravar um mapa).
        receberSelectMapa :: (Bool, Int), -- ^ Indica se a aplicação está a receber inputs numa caixa de seleção.
        mapasGuardados :: [String] -- ^ Mapas gravados no diretório da aplicação.
    } deriving Show

{- | Estado inical do editor de mapas. -}
estadoEditorInicial :: EstadoEditor
estadoEditorInicial = EstadoEditor (replicate alturaMapaEditor Nada) 0 (0,0,0.75) 0 0 (0, 0) False "" (False, 0) []

{- | A função 'drawEditor' desenha o editor de mapas. -}
drawEditor :: EstadoEditor -- ^ Estado do editor de mapas a ser desenhado.
    -> [[Picture]] -- ^ Imagens do terreno e obstáculos.
    -> [[Picture]] -- ^ Imagens do UI do editor.
    -> Picture -- ^ Retorna a imagem do estado do editor.
drawEditor estado@(EstadoEditor chunks modo (cx,cy, scale) barra item obj _ _ _ _) ps ts = Pictures [Scale scale scale $ Translate (fromIntegral cx) (fromIntegral cy) $ Pictures $ drawChunks (ps !! 0) (ps !! 1) chunks (0,400)++[desenhaCursor obj (head (ts !! 3))], drawUIEditor estado ts]

{- | A função 'drawUIEditor' desenha a interface do editor de mapas. -}
drawUIEditor :: EstadoEditor -- ^ Estado do editor de mapas a ser desenhado.
    -> [[Picture]] -- ^ Imagens do UI do editor.
    -> Picture -- ^ Retorna a imagem da interface.
drawUIEditor (EstadoEditor chunks modo (cx,cy, scale) barra item _ receber t (select, n) ns) ps = Pictures $ [if barra == 0 then head ps !! item else (ps !! 1) !! item, head $ ps !! 2] ++ if receber then [caixaInput t $ head (ps !! 4) ] else if select then [caixaSelect ns n ((ps !! 4) !! 1)] else []

{- | A função 'caixaInput' desenha a interface da caixa de input de texto (quando o utilizador quer gravar um mapa tem de escrever o seu nome). -}
caixaInput :: String -- ^ 'String' do que já foi escrito.
    -> Picture -- ^ Imagem do fundo da caixa.
    -> Picture -- ^ Retorna a imagem da caixa com o input dentro.
caixaInput s p = Pictures [p, Translate (-600) (-70) $ Color white $ Text s]

{- | A função 'caixaSelect' desenha a interface da caixa de escolha (quando o utilizador quer escolher um mapa já feito). -}
caixaSelect :: [String] -- ^ Nomes dos mapas no diretório da aplicação.
    -> Int -- ^ Mapa selecionado.
    -> Picture -- ^ Fundo da caixa de select.
    -> Picture -- ^ Retorna a imagem da caixa com o nome dos mapas dentro.
caixaSelect s n ps =  Pictures (ps:formataSelects (map (Text) s) 520 n)

{- | A função __recursiva__ 'formataSelects' é auxiliar de 'caixaSelect' e espaça os nomes dos mapas uns dos outros na caixa. -}
formataSelects :: [Picture] -- ^ 'Text'os com os nomes dos mapas.
    -> Float -- ^ Y da label, diminui em cada iteração da função.
    -> Int -- Opção selecionada pelo jogador.
    -> [Picture] -- ^ Retorna os textos separados.
formataSelects [] _ _ = []
formataSelects (h:t) y i = if i == 0 then (Color white $ Scale 0.5 0.5 $ Translate (-900) y h):formataSelects t (y-150) (i-1) else (Scale 0.5 0.5 $ Color (greyN 0.4) $ Translate (-900) y h):formataSelects t (y-150) (i-1)

{- | A função 'desenhaCursor' desenha o cursor do utilizador no mapa. -}
desenhaCursor :: Coordenadas -- ^ Coordenadas do cursor.
    -> Picture -- ^ Imagem do cursor.
    -> Picture -- ^ Imagem do cursor posicionada na tela.
desenhaCursor (x,y) = Translate (fromIntegral (x * 50 + y * (-50))) (400 - 25 * fromIntegral y - 25 * fromIntegral x)

{- | A função 'drawChunks' desenha cada linha do mapa a ser editado. -}
drawChunks :: [Picture] -- ^ Imagens dos terrenos
    -> [Picture] -- ^ Imagens dos obstáculos.
    -> [Chunk] -- ^ Linhas do mapa a ser editado.
    -> (Float,Float) -- ^ (x,y) variam a cada iteração da função para desenhar a linha de forma isométrica.
    -> [Picture] -- ^ Imagens das linhas.
drawChunks _ _ [] _ = []
drawChunks ps psobs (Nada:t) (x,y) = Pictures (drawLinhaNada ps larguraMapaEditor (x,y)):drawChunks ps psobs t (x - 50, y - 25)
drawChunks ps psobs ((Linha linha@(_, obs)):t) (x,y) = Pictures (drawLinha ps larguraMapaEditor linha (x, y) ++ drawLinhaObstaculo psobs obs (x, y) 0 0 False):drawChunks ps psobs t (x - 50, y - 25)

{- | A função 'drawLinhaNada' desenha linhas sem terreno. -}
drawLinhaNada :: [Picture] -- ^ Imagens dos terrenos.
    -> Largura -- ^ Largura do mapa a ser desenhado.
    -> (Float,Float) -- ^ (x,y) variam a cada iteração da função para desenhar a linha de forma isométrica.
    -> [Picture] -- ^ Imagens da linha.
drawLinhaNada _ 0 _ = []
drawLinhaNada ps lar (x,y) = drawSegmento  (ps !! 3) (x,y):drawLinhaNada ps (lar - 1) (x+50,y-25)

{- | A função 'editaChunk' recebe um 'Chunk' e reliza a sua edição relativamente aos inputs do utilizador. -}
editaChunk :: [Chunk] -- ^ Mapa a ser editado.
    -> Coordenadas -- ^ Coordenadas onde o utilizador realizou a edição.
    -> Int -- ^ Tipo de edição (alteração de terreno, adição de obstáuculo)
    -> Int -- ^ ID do objeto.
    -> [Chunk] -- ^ Mapa pós edição.
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

{- | A função 'nomeMapasGuardados' retorna os nomes de todos os mapas presentes no diretório do jogo. -}
nomeMapasGuardados :: IO [String]
nomeMapasGuardados = do
                        dirAtual <- getCurrentDirectory
                        all <- getDirectoryContents dirAtual
                        let filtered = filter (isSuffixOf ".mapa") all
                        return filtered

{- | A função 'leChunks' lê um ficheiro .mapa e faz parse de uma lista de @[Chunk]@. -}
leChunks :: String -- ^ Nome do ficheiro.
    -> IO [Chunk] -- ^ Chunks carregados.
leChunks n = do
    putStrLn $ "O mapa selecionado ("++n++") foi carregado"
    ficheiro <- readFile n
    return (read ficheiro)

{- | A função 'chunksParaMapa' tranforma uma lista de Chunks num 'Mapa' (usado para o utilizador poder jogar o mapa que está a editar). -}
chunksParaMapa :: [Chunk] -- ^ Lista de Chunks a ser transformada.
    -> Mapa -- ^ Mapa gerado.
chunksParaMapa chunks = Mapa larguraMapaEditor (map chunkParaLinha chunks)

{- | A função 'chunkParaLinha' é auxiliar de 'chunksParaMapa', transforma um Chunk numa Linha do tipo @(Terreno, [Obstaculo])@. -}
chunkParaLinha :: Chunk -- ^ Chunk a ser transformado.
    -> (Terreno, [Obstaculo]) -- ^ Linha gerada. 
chunkParaLinha (Linha l) = l

{- | A função 'chunksProntos' indica se uma lista de chunks pode ser transformada numa lista de linhas (não pode ter linhas sem terreno). -}
chunksProntos :: [Chunk] -- ^ Lista de chunks.
    -> Bool -- ^ Indica se a linha pode ser transformada.
chunksProntos [] = True
chunksProntos (Nada:t) = False
chunksProntos (_:t) = chunksProntos t