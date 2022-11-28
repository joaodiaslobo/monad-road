{- |
Module      : Tarefa3_2022li1g002
Description : Movimentação do personagem e obstáculos
Copyright   : João d'Araújo Dias Lobo <a104356@alunos.uminho.pt>
              Rita da Cunha Camacho <a104439@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g002 where

import LI12223

{- | A função __não recursiva__ 'animaJogo' movimenta os obstáculos (de acordo com a velocidade do terreno em que se encontram) e a personagem, de acordo com a jogada dada.

=== Exemplo de utilização:

>>>animaJogo (Jogo (Jogador (2,2)) (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])) (Move Esquerda)
Jogo (Jogador (1,2)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada (-1),[Nenhum,Nenhum,Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore])])
-}
animaJogo :: Jogo -- ^Estado do jogo antes da jogada.
    -> Jogada -- ^A jogada em si, 'Nenhum' ou 'Move' 'Direcao'.
    -> Jogo -- ^Jogo após a jogada dada.
animaJogo (Jogo p (Mapa l ls)) j = Jogo jogador (Mapa l linhas)
    where jogador = moveJogador p j ls; linhas = animaObstaculos ls jogador

{- | A função __não recursiva__ 'moveJogador' recebe 'Jogador' com as 'Coordenadas' antes das execução da função, uma 'Jogada' e a lista de linhas do mapa. Para o jogador se mover, têm de ser respeitadas as seguintes condições:

* O jogador __não__ pode sair do mapa (exceto quando está num tronco)
* O jogador __não__ pode ir para "cima" de árvores, ou seja, quando há uma árvore no caminho do jogador este fica parado no lugar inicial
* Quando a jogada é 'Parado' e o jogador se encontra na posição do tronco, este move-se junto com o tronco (|v| unidades na direção do rio)
* Quando o jogador se move horizontalmente e se encontra num tronco, a sua posição resultante em x será a soma do vetor v com (x ± 1)

=== Exemplo de utilização:

>>>moveJogador (Jogador (1,1)) (Move Cima) [(Relva, [Arvore, Nenhum, Arvore]),(Relva, [Nenhum,Nenhum,Nenhum])]
Jogador (1,0)

>>>moveJogador (Jogador (0,1)) (Move Cima) [(Relva, [Arvore, Nenhum, Arvore]),(Relva, [Nenhum,Nenhum,Nenhum])]
Jogador (0,1)
-}
moveJogador :: Jogador -- ^Estado do jogador antes da jogada.
    ->Jogada -- ^A jogada em si, 'Nenhum' ou 'Move' 'Direcao'.
    -> [(Terreno, [Obstaculo])] -- ^As linhas do mapa.
    -> Jogador -- ^Estado do jogador pós jogada.
moveJogador (Jogador (x,y)) j p =
    case j of
        Parado -> 
            case fst (p !! y) of
                Rio v -> if (snd (p !! y) !! x) == Tronco then Jogador (x+v,y) else Jogador (x,y)
                _ -> Jogador (x,y)
        Move Cima -> 
            case fst (p !! y) of
                Rio v -> if  (y-1 < 0) || ((snd (p !! (y-1)) !! x) == Arvore) then Jogador (x+v,y) else Jogador (x,y-1)
                _ -> if  (y-1 < 0) || ((snd (p !! (y-1)) !! x) == Arvore) then Jogador (x,y) else Jogador (x,y-1)
        Move Baixo ->
            case fst (p !! y) of
                Rio v -> if (y+1 >= length p) || ((snd (p !! (y+1)) !! x) == Arvore) then Jogador (x+v,y) else Jogador (x,y+1)
                _ -> if (y+1 >= length p) || ((snd (p !! (y+1)) !! x) == Arvore) then Jogador (x,y) else Jogador (x,y+1)
        Move Direita ->
            case fst (p !! y) of
                Rio v -> if (snd (p !! y) !! x) == Tronco then Jogador (x+1+v,y) else Jogador (x+1, y)
                _ -> if (x+1 >= length (snd (head p))) || (snd (p !! y) !! (x+1)) == Arvore then Jogador (x,y) else Jogador (x+1,y)
        Move Esquerda -> 
            case fst (p !! y) of
                Rio v -> if (snd (p !! y) !! x) == Tronco then Jogador (x-1+v,y) else Jogador (x-1, y)
                _ -> if (x-1 < 0) || (snd (p !! y) !! (x-1)) == Arvore then Jogador (x,y) else Jogador (x-1,y)

{- | A função __recursiva__ 'animaObstaculos' recebe uma lista de linhas do mapa e, com ajuda da função auxiliar 'animaObstaculoAux' e através da velocidade v do terreno, move todos os elementos da lista de obstáculos |v| unidades na direção da velocidade.

=== Exemplo de utilização:

>>>animaObstaculos [(Rio (-1), [Nenhum,Tronco,Tronco,Nenhum]),(Estrada 2, [Carro,Carro,Nenhum,Nenhum]),(Rio 1, [Tronco,Tronco,Nenhum,Nenhum])]
[(Rio (-1),[Tronco,Tronco,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Nenhum,Carro,Carro]),(Rio 1,[Nenhum,Tronco,Tronco,Nenhum])]
-}
animaObstaculos :: [(Terreno, [Obstaculo])] -- ^Linhas do mapa antes da animação.
    -> Jogador -- ^Estado do jogador pós movimentos.
    -> [(Terreno, [Obstaculo])] -- ^Linhas do mapa depois do movimento dos obstáculos.
animaObstaculos [] _ = []
animaObstaculos (l:t) (Jogador (x,y)) = 
    case fst l of
        Relva -> l:animaObstaculos t  (Jogador (x,y-1))
        Rio v -> (Rio v, animaObstaculoAux v (snd l) 0 x (y==0)):animaObstaculos t (Jogador (x,y-1))
        Estrada v -> (Estrada v, animaObstaculoAux v (snd l) 0 x (y==0)):animaObstaculos t (Jogador (x,y-1)) 

{- | A função auxiliar __recursiva__ 'animaObstaculoAux' recebe uma velocidade e uma lista de obstáculos, com esses valores "desvia" os elementos da lista pelo valor da velocidade.

=== Exemplo de utilização:
:animaObstaculos t (x,y-1)
>>>animaObstaculoAux (-1) [Nenhum,Tronco,Tronco,Nenhum] 0
[Tronco,Tronco,Nenhum,Nenhum]
-}
animaObstaculoAux :: Velocidade -- ^Velocidade do terreno a ser animado.
    -> [Obstaculo] -- ^Lista de obstáculos a ser movimentados.
    -> Int -- ^Contador.
    -> Int -- ^x do Jogador
    -> Bool -- ^Indica se o jogador está nesta linha
    -> [Obstaculo] -- ^Obstáculos pós transformação no x.
animaObstaculoAux v o n x b
    | ((o !! x) == Carro) && b = o
    | v > 0 =
        if n < v then animaObstaculoAux v (last o:init o) (n+1) x b else o
    | v < 0 =
        if n < abs v then animaObstaculoAux v (tail o++[head o]) (n+1) x b else o
    | otherwise = o