module Visualizador where

import LI12223
import Tarefa2_2022li1g002
import Tarefa3_2022li1g002
import Tarefa4_2022li1g002

visualizaEmoji :: Jogo -> IO()
visualizaEmoji j = putStrLn (visualizaEmojiAux j 0)

visualizaEmojiAux :: Jogo -> Int -> String
visualizaEmojiAux (Jogo (Jogador (x,y)) (Mapa l [])) _ = ""
visualizaEmojiAux (Jogo (Jogador (x,y)) (Mapa l (h:t))) n = if n==y then visualizaObstaculos h x++"\n"++visualizaEmojiAux (Jogo (Jogador (x,y)) (Mapa l t)) (n+1) else visualizaObstaculos h (-1)++"\n"++visualizaEmojiAux (Jogo (Jogador (x,y)) (Mapa l t)) (n+1)

visualizaObstaculos :: (Terreno, [Obstaculo]) 
    -> Int -- ^Caso o jogador esteja nesta linha, a sua coordenada x (se não esiver na linha passa-se como argumento qualquer número negativo).
    -> String
visualizaObstaculos (_, []) _ = ""
visualizaObstaculos (t, h:tt) n = if n==0 then playerEmoji++visualizaObstaculos (t,tt) (n-1) else
    case t of
        Relva -> if h == Nenhum then relvaEmoji++visualizaObstaculos (t, tt) (n-1) else arvoreEmoji++visualizaObstaculos (t, tt) (n-1)
        Estrada _ -> if h == Nenhum then estradaEmoji++visualizaObstaculos (t, tt) (n-1) else carroEmoji++visualizaObstaculos (t, tt) (n-1)
        Rio _ -> if h == Nenhum then rioEmoji++visualizaObstaculos (t, tt) (n-1) else troncoEmoji++visualizaObstaculos (t, tt) (n-1)

simulaJogo :: Jogo -> Jogada -> IO()
simulaJogo jogo jogada = let novo = animaJogo jogo jogada in do
    if jogoTerminou novo
        then 
            putStrLn "Fim do jogo! 🐣" 
    else do
        putStr "\ESC[2J" -- Em sistemas UNIX esta sequência de caracteres limpa a tela do terminal.
        visualizaEmoji novo
        putStrLn "Insira a próxima jogada (⬆ w, ⬇ s, ⬅ a, ➡ d, 🔄 p):"
        j <- getLine
        case j of
            "w" -> simulaJogo novo (Move Cima) 
            "a" -> simulaJogo novo (Move Esquerda) 
            "s" -> simulaJogo novo (Move Baixo) 
            "d" -> simulaJogo novo (Move Direita) 
            "p" -> simulaJogo novo Parado


geraMapa :: Largura -> Int -> Int -> Mapa
geraMapa l a r = let randoms = obterRandoms r a in geraMapaAux randoms l

geraMapaAux :: [Int] -> Largura -> Mapa
geraMapaAux [] l = Mapa l [(Relva, replicate l Nenhum)] 
geraMapaAux (h:t) l = let ss = mod h 100 in estendeMapa (geraMapaAux t l) ss


main = do
    putStr "\ESC[2J"
    putStrLn "### Visualizador auxiliar para o projeto de LI1 ###"
    putStrLn "Selecione uma opção\n(0) - Simular jogo pelo terminal\n(1) - Gerar e visualizar um mapa no terminal"
    e <- getLine
    let escolha = read e :: Int 
    putStrLn "Insira a largura do Mapa:"
    l <- getLine
    let largura = read l :: Int
    putStrLn "Insira a altura do Mapa:"
    a <- getLine
    let altura = read a :: Int
    putStrLn "Insira um número inteiro de [1,100] (random):"
    s <- getLine
    let seed = read s :: Int
    putStrLn "## (...) ##"
    if escolha == 1 then visualizaEmoji (Jogo (Jogador (-10,-10)) (geraMapa largura altura seed)) else simulaJogo (Jogo (Jogador (div largura 2,altura)) (geraMapa largura altura seed)) Parado
    
playerEmoji = "🐤"
relvaEmoji = "🟩"
arvoreEmoji = "🌳"
estradaEmoji = "⬛"
carroEmoji = "🚗"
rioEmoji = "🟦"
troncoEmoji = "🪵"