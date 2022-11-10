module Visualizador where

import LI12223
import Tarefa2_2022li1g002

relvaEmoji = "🟩"
arvoreEmoji = "🌳"
estradaEmoji = "⬛"
carroEmoji = "🚗"
rioEmoji = "🟦"
troncoEmoji = "🪵"

geraMapa :: Largura -> Int -> Int -> Mapa
geraMapa l a r = let randoms = obterRandoms r a in geraMapaAux randoms l

geraMapaAux :: [Int] -> Largura -> Mapa
geraMapaAux [] l = Mapa l [(Relva, replicate l Nenhum)] 
geraMapaAux (h:t) l = let ss = mod h 100 in estendeMapa (geraMapaAux t l) ss

visualizaEmoji :: Jogo -> IO()
visualizaEmoji j = putStrLn (visualizaEmojiAux j)

visualizaEmojiAux :: Jogo -> String
visualizaEmojiAux (Jogo (Jogador (x,y)) (Mapa l [])) = ""
visualizaEmojiAux (Jogo (Jogador (x,y)) (Mapa l (h:t))) = visualizaObstaculos h++"\n"++visualizaEmojiAux (Jogo (Jogador (x,y)) (Mapa l t))

visualizaObstaculos :: (Terreno, [Obstaculo]) -> String
visualizaObstaculos (_, []) = ""
visualizaObstaculos (t, h:tt) =
    case t of
        Relva -> if h == Nenhum then relvaEmoji++visualizaObstaculos (t, tt) else arvoreEmoji++visualizaObstaculos (t, tt)
        Estrada _ -> if h == Nenhum then estradaEmoji++visualizaObstaculos (t, tt) else carroEmoji++visualizaObstaculos (t, tt)
        Rio _ -> if h == Nenhum then rioEmoji++visualizaObstaculos (t, tt) else troncoEmoji++visualizaObstaculos (t, tt)

main = do 
    putStrLn "### Visualizador auxiliar para o projeto de LI ###"
    putStrLn "Insira a largura do Mapa:"
    l <- getLine
    let largura = read l :: Int
    putStrLn "Insira a altura do Mapa:"
    a <- getLine
    let altura = read a :: Int
    putStrLn "Insira um número inteiro de [1,100] (random)"
    s <- getLine
    let seed = read s :: Int
    putStrLn "## A iniciar o jogo"
    visualizaEmoji (Jogo (Jogador (0,0)) (geraMapa largura altura seed))