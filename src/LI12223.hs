{- |
Module      : LI12223
Description : Módulo auxiliar para LI1 22/23.
Copyright   : Manuel Barros <d13242@di.uminho.pt>
              Nelson Estevão <d12733@di.uminho.pt>
              Olga Pacheco <omp@di.uminho.pt>
              Xavier Pinho <d12736@di.uminho.pt>

Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2022/23.
 -}
module LI12223 (
  -- * Tipos de dados
  -- ** Básicos
  Coordenadas , Largura , Velocidade,
  -- ** Mapas
  Mapa(..), Terreno(..), Obstaculo(..),
    -- ** Jogo
  Jogo(..), Jogador(..), Direcao(..), Jogada(..)
  ) where

-- | Velocidade que irá afetar a movimentação dos 'Obstaculo's de um 'Mapa'.
type Velocidade = Int

{- | Cada linha de um 'Mapa' é um Terreno em particular contendo 'Obstaculo's.

As linhas do tipo 'Rio' ou 'Estrada' têm a propriedade 'Velocidade' que indicam a velocidade de deslocamento dos obstáculos. Podendo esta ser negativa, indicando assim a sua direção.
-}
data Terreno
  = Rio Velocidade
  | Estrada Velocidade
  | Relva
  deriving (Show, Read, Eq)

-- | Um Obstáculo numa linha de um 'Mapa'.
data Obstaculo
  = Nenhum -- ^ a ausência de obstáculos
  | Tronco -- ^ os troncos deslizam apenas em 'Rio'
  | Carro -- ^ os carros movimentam-se apenas em 'Estrada'
  | Arvore -- ^ as árvores são um obstáculo fixo que não se move e apenas são possíveis em 'Relva'
  deriving (Show, Read, Eq)

-- | Comprimento de um 'Mapa'.
type Largura = Int

-- | O Mapa que constituí o 'Jogo'.
data Mapa =
  Mapa Largura [(Terreno, [Obstaculo])]
  deriving (Show, Read, Eq)

-- | Par de coordenadas de uma posição no 'Mapa'.
type Coordenadas = (Int, Int)

-- | O Jogador define o personagem controlado no 'Jogo'.
newtype Jogador =
  Jogador Coordenadas
  deriving (Show, Read, Eq)

-- | Definição base de um jogo.
data Jogo =
  Jogo
    Jogador -- ^ o personagem do jogo
    Mapa -- ^ o mapa em que se está a jogar
  deriving (Show, Read, Eq)

-- | Direção de uma 'Jogada' feita por um 'Jogador' no 'Mapa'.
data Direcao
  = Cima
  | Baixo
  | Esquerda
  | Direita
  deriving (Show, Read, Eq)

-- | As acções que podem ser tomadas pelo 'Jogador' em cada estado do 'Jogo'.
data Jogada
  = Parado -- ^ tipo que define a ausência de uma acção do 'Jogador'
  | Move Direcao -- ^ um movimento do jogador numa determinada 'Direcao'
  deriving (Show, Read, Eq)
