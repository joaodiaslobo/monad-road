{- |
Module      : Visualizador
Description : Simulador de jogo e gerador de mapa com o objetivo de melhor visualização do trabalho criado
Copyright   : João d'Araújo Dias Lobo <a104356@alunos.uminho.pt>
              Rita da Cunha Camacho <a104439@alunos.uminho.pt>

Módulo extra para a realização do Visualizador do projeto de LI1 em 2022/23.
-}
module Visualizador where

import LI12223
import Tarefa2_2022li1g002
import Tarefa3_2022li1g002
import Tarefa4_2022li1g002

{- | A função 'main' tem como objetivo recolher o /input/ do utilizador e configurar o simulador seguindo as informações indicadas.

=== Guia de utilização
O visualizador tem dois modos:

* O simulador de jogo, como o nome indica, através das funções definidas nas outras tarefas, irá iniciar um jogo representado por /emojis/ no terminal 
* O gerador de mapa foi desenvolvido com o interesse de verificar graficamente se todas as condições da geração de mapa são respeitadas, tornando tal processo mais acessível e intuitivo graças à representação visual

Para utilizar o visualizador, recomenda-se que execute as seguintes instruções:

- Na pasta principal do repositório:

>>> ghci -i="src" src/Visualizador.hs
>>> main
### Visualizador auxiliar para o projeto de LI1 ###
Selecione uma opção
(0) - Simular jogo pelo terminal
(1) - Gerar e visualizar um mapa no terminal

- Insira o número da operação que pretende e siga os passos que aparecem na tela.

Exemplo da simulação de um jogo de largura __6__, altura __10__ e /seed/ __5__

![Exemplo](https://i.imgur.com/y22Pacx.png)
-}
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

{- | A função __recursiva__ 'visualizaEmoji', através da função auxiliar 'visualizaEmojiAux', traduz objetos de jogo (obstáculos e jogador) em /emojis/.-}    
visualizaEmoji :: Jogo -- ^Estado do jogo a ser representado através de /emojis/.
    -> IO() -- ^/Output/ do mapa e jogador (se aplicável) no terminal.
visualizaEmoji j = putStrLn (visualizaEmojiAux j 0)

{- | A função __recursiva__ 'visualizaEmojiAux', através da função auxiliar 'visualizaObstaculos', transforma as linhas do mapa em /emojis/.-}
visualizaEmojiAux :: Jogo -- ^Estado do jogo a ser representado através de /emojis/.
    -> Int -- ^Contador para, se aplicável, encontrar a linha onde se situa o jogador.
    -> String -- ^Combinação de /emojis/ que representam as linhas do mapa.
visualizaEmojiAux (Jogo (Jogador (x,y)) (Mapa l [])) _ = ""
visualizaEmojiAux (Jogo (Jogador (x,y)) (Mapa l (h:t))) n = if n==y then visualizaObstaculos h x++"\n"++visualizaEmojiAux (Jogo (Jogador (x,y)) (Mapa l t)) (n+1) else visualizaObstaculos h (-1)++"\n"++visualizaEmojiAux (Jogo (Jogador (x,y)) (Mapa l t)) (n+1)

{- | A função __recursiva__ 'visualizaObstaculos' transforma uma só linha do mapa em /emojis/.-}
visualizaObstaculos :: (Terreno, [Obstaculo]) -- ^A linha do mapa a ser traduzida.
    -> Int -- ^Caso o jogador esteja nesta linha, a sua coordenada x (se não esiver na linha passa-se como argumento qualquer número negativo).
    -> String -- ^Combinação de /emojis/ que representam uma linha do mapa.
visualizaObstaculos (_, []) _ = ""
visualizaObstaculos (t, h:tt) n = if n==0 then playerEmoji++visualizaObstaculos (t,tt) (n-1) else
    case t of
        Relva -> if h == Nenhum then relvaEmoji++visualizaObstaculos (t, tt) (n-1) else arvoreEmoji++visualizaObstaculos (t, tt) (n-1)
        Estrada _ -> if h == Nenhum then estradaEmoji++visualizaObstaculos (t, tt) (n-1) else carroEmoji++visualizaObstaculos (t, tt) (n-1)
        Rio _ -> if h == Nenhum then rioEmoji++visualizaObstaculos (t, tt) (n-1) else troncoEmoji++visualizaObstaculos (t, tt) (n-1)

{- | A função __recursiva__ 'simulaJogo' utiliza funções definidas nas tarefas 3 e 4 como auxiliares para simular um jogo de /Crossy Road/.-}
simulaJogo :: Jogo -- ^Estado do jogo antes de ser executada uma jogada.
    -> Jogada -- ^A jogada /input/ do utilizador.
    -> IO() -- ^/Output/ do mapa e do jogador no terminal.
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

{- | 🐤-}
playerEmoji = "🐤"

{- | 🟩-}
relvaEmoji = "🟩"

{- | 🌳-}
arvoreEmoji = "🌳"

{- | ⬛-}
estradaEmoji = "⬛"

{- | 🚗-}
carroEmoji = "🚗"

{- | 🟦-}
rioEmoji = "🟦"

{- | 🪵-}
troncoEmoji = "🪵"