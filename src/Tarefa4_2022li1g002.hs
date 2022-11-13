{- |
Module      : Tarefa4_2022li1g002
Description : Determinar se o jogo terminou
Copyright   : João d'Araújo Dias Lobo <a104356@alunos.uminho.pt>
              Rita da Cunha Camacho <a104439@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g002 where

import LI12223
{- | A função __não recursiva__ 'jogoTerminou' irá devolver-nos um 'Bool', que dará o valor de verdade da afirmação presente no próprio nome da função, necessitando então a mesma de receber um dado Jogo. O objetivo desta função é, através de certas condições impostas, verificar se o jogador perdeu e, consequentemente, se o jogo terminou. Caso contrário, o jogo continua, pois o jogador não cometeu nenhuma das seguintes infrações:

* O jogador encontra-se fora do mapa
* O jogador encontra-se na água
* O jogador encontra-se “debaixo” de um carro, ou seja, nas mesmas coordenadas de um carro 

=== Exemplos de utilização:

>>>jogoTerminou (Jogo (Jogador (2,0)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]))
True

>>>jogoTerminou (Jogo (Jogador (0,0)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]))
False

>>>jogoTerminou (Jogo (Jogador (4,1)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]))
True

>>>jogoTerminou (Jogo (Jogador (2,1)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]))
False

>>>jogoTerminou (Jogo (Jogador (6,4)) (Mapa 5 [(Rio 3, [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]))
True
-}

jogoTerminou :: Jogo -- ^Jogo a avaliar, onde estão incluídas as coordenadas do 'Jogador' e os dados do 'Mapa'.
    -> Bool -- ^Será 'True' quando o Jogador perder, e 'False' quando ainda não tiver perdido.
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l p))
    | x < 0 || x >= length (snd (head p)) || y < 0 || y >= length p = True
    | otherwise = let terreno = fst (p !! y); obstaculo = snd (p !! y) !! x in
        case terreno of
            Relva -> False
            Estrada _ -> obstaculo == Carro
            Rio _ -> obstaculo == Nenhum