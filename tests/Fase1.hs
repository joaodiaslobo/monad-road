module Main where

import           Control.Exception
import           Control.Monad
import           Data.List
import           LI12223
import           System.Directory    (createDirectoryIfMissing)
import           System.Exit
import           System.IO
import           Tarefa1_2022li1g002
import           Tarefa2_2022li1g002
import           Tarefa3_2022li1g002
import           Tarefa4_2022li1g002
import           Test.HUnit

-------------------------------------------------------------------------------
-- Helpers                                                                  --
-------------------------------------------------------------------------------

isRelva :: Terreno -> Bool
isRelva Relva = True
isRelva _     = False

isEstrada :: Terreno -> Bool
isEstrada (Estrada _) = True
isEstrada _           = False

isRio :: Terreno -> Bool
isRio (Rio _) = True
isRio _       = False

checkAll :: Ord t => [t -> Bool] -> [t] -> Bool
checkAll functions values =
  all (\(f, x) -> f x) $ zip functions values

checkMap :: Mapa -> Bool
checkMap (Mapa larg terrs) = all validTerrain terrs
  where
    validTerrain :: (Terreno, [Obstaculo]) -> Bool
    validTerrain (terr, obs) =
      all (`elem` validObstacles terr) obs &&
      larg == length obs &&
      (null obs || Nenhum `elem` obs) &&
      dimsOk limSupObstaculo (juntaExtremos (group obs))
    validObstacles :: Terreno -> [Obstaculo]
    validObstacles t = Nenhum : obs
      where
        obs =
          case t of
            Rio _ -> [Tronco]
            Estrada _ -> [Carro]
            Relva -> [Arvore]
    validTerrainPairs :: [Terreno] -> Bool
    validTerrainPairs ts =
      and (zipWith sameTypeOrOppositeDirection ts (tail ts)) &&
      dimsOk limSupTerreno (groupBy sameType ts)
    sameTypeOrOppositeDirection :: Terreno -> Terreno -> Bool
    sameTypeOrOppositeDirection (Rio v1) (Rio v2) = v1 * v2 <= 0
    sameTypeOrOppositeDirection _ _ = True
    sameType :: Terreno -> Terreno -> Bool
    sameType (Rio _) (Rio _) = True
    sameType (Estrada _) (Estrada _) = True
    sameType Relva Relva = True
    sameType _ _ = False
    juntaExtremos :: [[Obstaculo]] -> [[Obstaculo]]
    juntaExtremos os
      | length os > 1 && head (head os) == head (last os) =
        (head os ++ last os) : init (tail os)
      | otherwise = os
    dimsOk :: (a -> Int) -> [[a]] -> Bool
    dimsOk limSup = all dimOk
      where
        dimOk ls = limSup (head ls) == -1 || length ls <= limSup (head ls)
    limSupTerreno :: Terreno -> Int
    limSupTerreno (Rio _) = 4
    limSupTerreno (Estrada _) = 5
    limSupTerreno Relva = 5
    riosDirOp :: [Terreno] -> Bool
    riosDirOp ts = and $ zipWith terrenosSeguidosOk ts (tail ts)
    terrenosSeguidosOk :: Terreno -> Terreno -> Bool
    terrenosSeguidosOk (Rio v1) (Rio v2) = v1 * v2 <= 0 -- Dois terrenos seguidos não são válidos se são ambos rios com as mesmas direções
    terrenosSeguidosOk _ _ = True
    limSupObstaculo :: Obstaculo -> Int
    limSupObstaculo Nenhum = -1
    limSupObstaculo Tronco = 5
    limSupObstaculo Arvore = -1
    limSupObstaculo Carro = 3

-------------------------------------------------------------------------------
-- Fixtures                                                                  --
-------------------------------------------------------------------------------

-- Mapas Inválidos

imapa01_estrada1 :: Mapa
imapa01_estrada1 = Mapa
  3
  [ (Rio 1    , [Nenhum, Tronco, Tronco])
  , (Estrada 1, [Nenhum, Tronco, Nenhum])
  , (Relva    , [Nenhum, Arvore, Nenhum])
  ]

imapa01_estrada2 :: Mapa
imapa01_estrada2 = Mapa
  3
  [ (Rio 1    , [Nenhum, Tronco, Tronco])
  , (Estrada 1, [Nenhum, Nenhum, Arvore])
  , (Relva    , [Nenhum, Arvore, Nenhum])
  ]

imapa01_relva1 :: Mapa
imapa01_relva1 = Mapa
  3
  [ (Rio 1    , [Nenhum, Tronco, Tronco])
  , (Estrada 1, [Nenhum, Nenhum, Carro])
  , (Relva    , [Nenhum, Arvore, Tronco])
  ]

imapa01_relva2 :: Mapa
imapa01_relva2 = Mapa
  3
  [ (Rio 1    , [Nenhum, Tronco, Tronco])
  , (Estrada 1, [Nenhum, Nenhum, Carro])
  , (Relva    , [Nenhum, Arvore, Carro])
  ]

imapa01_rio1 :: Mapa
imapa01_rio1 = Mapa
  3
  [ (Rio 1    , [Nenhum, Carro, Tronco])
  , (Estrada 1, [Nenhum, Nenhum, Carro])
  , (Relva    , [Nenhum, Arvore, Arvore])
  ]

imapa01_rio2 :: Mapa
imapa01_rio2 = Mapa
  3
  [ (Rio 1    , [Arvore, Nenhum, Tronco])
  , (Estrada 1, [Nenhum, Nenhum, Carro])
  , (Relva    , [Nenhum, Arvore, Arvore])
  ]

imapa02_1 :: Mapa
imapa02_1 = Mapa
  3
  [ (Rio 1, [Tronco, Nenhum, Tronco])
  , (Rio 1, [Nenhum, Nenhum, Tronco])
  , (Relva, [Nenhum, Arvore, Arvore])
  ]

imapa02_2 :: Mapa
imapa02_2 = Mapa
  3
  [ (Rio (-1), [Tronco, Nenhum, Tronco])
  , (Rio (-1), [Nenhum, Nenhum, Tronco])
  , (Relva   , [Nenhum, Arvore, Arvore])
  ]

imapa02_3 :: Mapa
imapa02_3 = Mapa
  3
  [ (Rio (-1), [Tronco, Nenhum, Tronco])
  , (Relva   , [Nenhum, Arvore, Arvore])
  , (Relva   , [Nenhum, Arvore, Arvore])
  , (Rio (-1), [Tronco, Nenhum, Tronco])
  , (Rio (-1), [Nenhum, Nenhum, Tronco])
  , (Rio 1   , [Nenhum, Nenhum, Tronco])
  , (Relva   , [Nenhum, Arvore, Arvore])
  ]

imapa03_1 :: Mapa
imapa03_1 = Mapa
  8
  [ (Relva    , [Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Arvore, Arvore, Nenhum, Nenhum, Nenhum])
  , (Rio 1    , [Nenhum, Nenhum, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Arvore, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Carro, Nenhum])
  , (Rio 1    , [Nenhum, Tronco, Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum])
  , (Rio (-1) , [Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Arvore, Arvore, Nenhum, Nenhum, Nenhum])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Carro, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Arvore, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  ]

imapa03_2 :: Mapa
imapa03_2 = Mapa
  8
  [ (Relva    , [Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Arvore, Arvore, Nenhum, Nenhum, Nenhum])
  , (Rio 1    , [Nenhum, Nenhum, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Arvore, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Carro, Nenhum])
  , (Rio 1    , [Nenhum, Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum, Nenhum])
  , (Rio (-1) , [Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Arvore, Arvore, Nenhum, Nenhum, Nenhum])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Carro, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Arvore, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  ]

imapa04_1 :: Mapa
imapa04_1 = Mapa
  6
  [ (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Estrada 1, [Nenhum, Carro, Nenhum, Nenhum, Nenhum, Carro])
  , (Estrada 1, [Nenhum, Carro, Carro, Carro, Carro, Nenhum])
  , (Rio 1    , [Nenhum, Nenhum, Tronco, Tronco, Nenhum, Nenhum])
  , (Rio (-1) , [Nenhum, Tronco, Tronco, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  ]

imapa04_2 :: Mapa
imapa04_2 = Mapa
  5
  [ (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  , (Relva    , [Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Estrada 1, [Carro, Nenhum, Nenhum, Nenhum, Carro])
  , (Estrada 1, [Carro, Carro, Carro, Nenhum, Carro])
  , (Rio 1    , [Nenhum, Tronco, Tronco, Nenhum, Nenhum])
  , (Rio (-1) , [Tronco, Tronco, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  ]

imapa05_1 :: Mapa
imapa05_1 = Mapa
  3
  [ (Rio 1   , [Tronco, Tronco, Tronco])
  , (Estrada (-1), [Nenhum, Carro, Nenhum])
  , (Relva   , [Nenhum, Arvore, Nenhum])
  ]

imapa05_2 :: Mapa
imapa05_2 = Mapa
  3
  [ (Rio 1   , [Nenhum, Tronco, Tronco])
  , (Estrada (-1), [Nenhum, Carro, Nenhum])
  , (Relva   , [Arvore, Arvore, Arvore])
  ]

imapa05_3 :: Mapa
imapa05_3 = Mapa
  3
  [ (Rio 1   , [Nenhum, Nenhum, Tronco])
  , (Estrada (-1), [Carro, Carro, Carro])
  , (Relva   , [Arvore, Nenhum, Arvore])
  ]

imapa06_1 :: Mapa
imapa06_1 = Mapa
  3
  [ (Rio 1   , [Nenhum, Tronco])
  , (Estrada (-1), [Carro, Nenhum])
  , (Relva   , [Arvore, Nenhum])
  ]

imapa06_2 :: Mapa
imapa06_2 = Mapa
  3
  [ (Rio 1   , [Nenhum, Nenhum, Tronco, Nenhum])
  , (Estrada (-1), [Carro, Carro, Carro, Nenhum])
  , (Relva   , [Arvore, Nenhum, Arvore, Nenhum])
  ]

imapa06_3 :: Mapa
imapa06_3 = Mapa
  4
  [ (Rio 1       , [Nenhum, Nenhum, Tronco, Nenhum])
  , (Estrada (-1), [Carro, Carro, Nenhum])
  , (Relva       , [Arvore, Nenhum, Arvore, Nenhum])
  ]

imapa07_1 :: Mapa
imapa07_1 = Mapa
  5
  [ (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  , (Relva    , [Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Rio 1    , [Nenhum, Tronco, Tronco, Nenhum, Nenhum])
  , (Rio (-1) , [Tronco, Tronco, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  ]

imapa07_2 :: Mapa
imapa07_2 = Mapa
  5
  [ (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  , (Relva    , [Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Rio 1    , [Nenhum, Tronco, Tronco, Nenhum, Nenhum])
  , (Rio (-1) , [Tronco, Tronco, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  ]

imapa07_3 :: Mapa
imapa07_3 = Mapa
  5
  [ (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  , (Relva    , [Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Rio (-1) , [Tronco, Tronco, Nenhum, Nenhum, Nenhum])
  , (Rio 1    , [Nenhum, Tronco, Tronco, Nenhum, Nenhum])
  , (Rio (-1) , [Tronco, Tronco, Nenhum, Nenhum, Nenhum])
  , (Rio 1    , [Nenhum, Tronco, Tronco, Nenhum, Nenhum])
  , (Rio (-1) , [Tronco, Tronco, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  ]

-- Mapas

mapa01 :: Mapa
mapa01 = Mapa
  3
  [ (Rio 1   , [Nenhum, Tronco, Tronco])
  , (Rio (-1), [Tronco, Tronco, Nenhum])
  , (Relva   , [Nenhum, Arvore, Nenhum])
  ]

mapa01' :: Mapa
mapa01' = Mapa
  3
  [ (Rio 1   , [Tronco, Nenhum, Tronco])
  , (Rio (-1), [Tronco, Nenhum, Tronco])
  , (Relva   , [Nenhum, Arvore, Nenhum])
  ]

mapa02 :: Mapa
mapa02 = Mapa
  5
  [ (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  , (Relva    , [Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum, Carro])
  , (Rio 1    , [Nenhum, Tronco, Tronco, Nenhum, Nenhum])
  , (Rio (-1) , [Tronco, Tronco, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  ]

mapa02' :: Mapa
mapa02' = Mapa
  5
  [ (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  , (Relva    , [Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Estrada 1, [Carro, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Rio 1    , [Nenhum, Nenhum, Tronco, Tronco, Nenhum])
  , (Rio (-1) , [Tronco, Nenhum, Nenhum, Nenhum, Tronco])
  , (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  ]

mapa03 :: Mapa
mapa03 = Mapa
  4
  [ (Estrada 1   , [Carro, Carro, Nenhum, Nenhum])
  , (Estrada 1   , [Nenhum, Carro, Carro, Nenhum])
  , (Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum])
  , (Rio 1       , [Nenhum, Tronco, Tronco, Nenhum])
  , (Rio (-1)    , [Tronco, Tronco, Nenhum, Nenhum])
  , (Relva       , [Nenhum, Arvore, Nenhum, Nenhum])
  , (Relva       , [Nenhum, Arvore, Arvore, Nenhum])
  , (Relva       , [Nenhum, Nenhum, Arvore, Nenhum])
  , (Relva       , [Nenhum, Nenhum, Arvore, Nenhum])
  ]

mapa03' :: Mapa
mapa03' = Mapa
  4
  [ (Estrada 1   , [Nenhum, Carro, Carro, Nenhum])
  , (Estrada 1   , [Nenhum, Nenhum, Carro, Carro])
  , (Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro])
  , (Rio 1       , [Nenhum, Nenhum, Tronco, Tronco])
  , (Rio (-1)    , [Tronco, Nenhum, Nenhum, Tronco])
  , (Relva       , [Nenhum, Arvore, Nenhum, Nenhum])
  , (Relva       , [Nenhum, Arvore, Arvore, Nenhum])
  , (Relva       , [Nenhum, Nenhum, Arvore, Nenhum])
  , (Relva       , [Nenhum, Nenhum, Arvore, Nenhum])
  ]

mapa03'' :: Mapa
mapa03'' = Mapa
  4
  [ (Estrada 1   , [Nenhum, Nenhum, Carro, Carro])
  , (Estrada 1   , [Carro, Nenhum, Nenhum, Carro])
  , (Estrada (-1), [Nenhum, Nenhum, Carro, Nenhum])
  , (Rio 1       , [Tronco, Nenhum, Nenhum, Tronco])
  , (Rio (-1)    , [Nenhum, Nenhum, Tronco, Tronco])
  , (Relva       , [Nenhum, Arvore, Nenhum, Nenhum])
  , (Relva       , [Nenhum, Arvore, Arvore, Nenhum])
  , (Relva       , [Nenhum, Nenhum, Arvore, Nenhum])
  , (Relva       , [Nenhum, Nenhum, Arvore, Nenhum])
  ]

mapa04 :: Mapa
mapa04 = Mapa
  5
  [ (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  , (Relva    , [Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Estrada 1, [Carro, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Rio 1    , [Nenhum, Nenhum, Tronco, Tronco, Nenhum])
  , (Rio (-1) , [Tronco, Nenhum, Nenhum, Nenhum, Tronco])
  , (Rio 1    , [Nenhum, Nenhum, Tronco, Tronco, Nenhum])
  , (Rio (-1) , [Tronco, Nenhum, Nenhum, Nenhum, Tronco])
  , (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  ]

mapa04' :: Mapa
mapa04' = Mapa
  5
  [ (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  , (Relva    , [Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
  , (Estrada 1, [Nenhum, Carro, Nenhum, Nenhum, Nenhum])
  , (Rio 1    , [Nenhum, Nenhum, Nenhum, Tronco, Tronco])
  , (Rio (-1) , [Nenhum, Nenhum, Nenhum, Tronco, Tronco])
  , (Rio 1    , [Nenhum, Nenhum, Nenhum, Tronco, Tronco])
  , (Rio (-1) , [Nenhum, Nenhum, Nenhum, Tronco, Tronco])
  , (Relva    , [Nenhum, Arvore, Nenhum, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Arvore])
  , (Relva    , [Nenhum, Nenhum, Arvore, Nenhum, Nenhum])
  ]

mapa05 :: Mapa
mapa05 = Mapa
  3
  [ (Rio 1   , [Tronco, Tronco, Nenhum])
  , (Rio (-1), [Tronco, Tronco, Nenhum])
  , (Rio 1   , [Nenhum, Tronco, Tronco])
  , (Rio (-1), [Tronco, Tronco, Nenhum])
  , (Relva   , [Nenhum, Arvore, Nenhum])
  ]

mapa05' :: Mapa
mapa05' = Mapa
  3
  [ (Rio 1   , [Nenhum, Tronco, Tronco])
  , (Rio (-1), [Tronco, Nenhum, Tronco])
  , (Rio 1   , [Tronco, Nenhum, Tronco])
  , (Rio (-1), [Tronco, Nenhum, Tronco])
  , (Relva   , [Nenhum, Arvore, Nenhum])
  ]

jogador01 :: Jogador
jogador01 = Jogador (0, 2)

jogo01 :: Jogo
jogo01 = Jogo jogador01 mapa01

jogo01' :: Jogo
jogo01' = Jogo (Jogador (0, 3)) mapa01'

-------------------------------------------------------------------------------
-- Testes T1                                                                 --
-------------------------------------------------------------------------------
testesT1_validos = TestLabel "#mapaValido" $ test
  [ "Mapa 01 é válido" ~: True ~=? mapaValido mapa01
  , "Mapa 1' é válido" ~: True ~=? mapaValido mapa01'
  , "Mapa 02 é válido" ~: True ~=? mapaValido mapa02
  , "Mapa 2' é válido" ~: True ~=? mapaValido mapa02'
  , "Mapa 03 é válido" ~: True ~=? mapaValido mapa03
  , "Mapa 3' é válido" ~: True ~=? mapaValido mapa03'
  , "Mapa 04 é válido" ~: True ~=? mapaValido mapa04
  , "Mapa 4' é válido" ~: True ~=? mapaValido mapa04'
  ]

testesT1_invalidos_1 = TestLabel "Verificar terrenos impróprios" $ test
  [ "Estrada não pode conter Tronco" ~: False ~=? mapaValido imapa01_estrada1
  , "Estrada não pode conter Arvore" ~: False ~=? mapaValido imapa01_estrada2
  , "Relva não pode conter Tronco" ~: False ~=? mapaValido imapa01_relva1
  , "Relva não pode conter Carro" ~: False ~=? mapaValido imapa01_relva2
  , "Rio não pode conter Carro" ~: False ~=? mapaValido imapa01_rio1
  , "Rio não pode conter Arvore" ~: False ~=? mapaValido imapa01_rio2
  ]

testesT1_invalidos_2 = TestLabel "Rios contíguos têm direcções opostas" $ test
  [ "Rios seguidos não podem ter ambos velocidade positiva" ~: False ~=? mapaValido imapa02_1
  , "Rios seguidos não podem ter ambos velocidade negativa" ~: False ~=? mapaValido imapa02_2
  , "Direções de rios têm de ser alternadas" ~: False ~=? mapaValido imapa02_3
  ]

testesT1_invalidos_3 = TestLabel "Verificar minímos e máximos de obstáculos consecutivos" $ test
  [ "Rio não pode ter mais do que 5 troncos consecutivos" ~: False ~=? mapaValido imapa03_1
  , "Rio não pode ter mais do que 5 troncos consecutivos circularmente" ~: False ~=? mapaValido imapa03_2
  , "Estrada não pode ter mais do que 4 carros consecutivos" ~: False ~=? mapaValido imapa04_1
  , "Estrada não pode ter mais do que 5 carros consecutivos circularmente" ~: False ~=? mapaValido imapa04_2
  , "Rio não pode ser constituído só por Tronco" ~: False ~=? mapaValido imapa05_1
  , "Relva não pode ser constituída só por Arvore" ~: False ~=? mapaValido imapa05_2
  , "Estrada não pode ser constituída só por Carro" ~: False ~=? mapaValido imapa05_3
  ]

testesT1_invalidos_4 = TestLabel "Verificar comprimento da lista de obstáculos" $ test
  [ "Comprimento dos obstáculos não pode ser menor" ~: False ~=? mapaValido imapa06_1
  , "Comprimento dos obstáculos não pode ser maior" ~: False ~=? mapaValido imapa06_2
  , "Comprimento dos obstáculos tem de ser sempre igual" ~: False ~=? mapaValido imapa06_3
  ]

testesT1_invalidos_5 = TestLabel "Verificar linhas repetidas consecutivas" $ test
  [ "Não pode haver mais do que 5 Estradas consecutivas" ~: False ~=? mapaValido imapa06_1
  , "Não pode haver mais do que 5 Relvas consecutivas" ~: False ~=? mapaValido imapa07_2
  , "Não pode haver mais do que 4 Rios consecutivos" ~: False ~=? mapaValido imapa07_3
  ]

testesT1_invalidos = TestLabel "#mapaValido" $ test
  [ testesT1_invalidos_1
  , testesT1_invalidos_2
  , testesT1_invalidos_3
  , testesT1_invalidos_4
  , testesT1_invalidos_5
  ]

testsT1 = TestLabel "Tarefa 1" $ test [testesT1_validos, testesT1_invalidos]

-------------------------------------------------------------------------------
-- Testes T2                                                                 --
-------------------------------------------------------------------------------
testesT2_terrenos = TestLabel "#proximosTerrenosValidos" $ test
  [ "Mapa 01" ~: True ~=? checkAll [isRio, isEstrada, isRelva] (proximosTerrenosValidos mapa01)
  , "Mapa 02" ~: True ~=? checkAll [isRio, isEstrada] (proximosTerrenosValidos mapa02)
  , "Mapa 03" ~: True ~=? checkAll [isRio, isEstrada, isRelva] (proximosTerrenosValidos mapa03)
  , "Mapa 04" ~: True ~=? checkAll [isRio, isEstrada] (proximosTerrenosValidos mapa04)
  , "Mapa 05" ~: True ~=? checkAll [isEstrada, isRelva] (proximosTerrenosValidos mapa05)
  ]

testesT2_obstaculos = TestLabel "#proximosObstaculosValidos" $ test
  [ "(Relva, [Nenhum, Arvore]) // 3" ~: [Nenhum, Arvore] ~=? sort (proximosObstaculosValidos 3 (Relva, [Nenhum, Arvore]))
  , "(Estrada 1, [Nenhum, Carro]) // 3" ~: [Nenhum, Carro] ~=? sort (proximosObstaculosValidos 3 (Estrada 1, [Nenhum, Carro]))
  , "(Rio 1, [Nenhum, Tronco]) // 3" ~: [Nenhum, Tronco] ~=? sort (proximosObstaculosValidos 3 (Rio 1, [Nenhum, Tronco]))
  -- , "(Rio 1, [Nenhum, Tronco, Tronco, Tronco, Tronco, Tronco]) // 7" ~: [Nenhum] ~=? sort (proximosObstaculosValidos 7 (Rio 1, [Nenhum, Tronco, Tronco, Tronco, Tronco, Tronco]))
  , "(Rio 1, [Nenhum, Tronco, Tronco, Tronco, Tronco]) // 7" ~: [Nenhum, Tronco] ~=? sort (proximosObstaculosValidos 7 (Rio 1, [Nenhum, Tronco, Tronco, Tronco, Tronco]))
  , "(Rio 1, [Tronco, Tronco]) // 3" ~: [Nenhum] ~=? sort (proximosObstaculosValidos 3 (Rio 1, [Tronco, Tronco]))
  -- , "(Estrada 1, [Nenhum, Carro, Carro, Carro]) // 5" ~: [Nenhum] ~=? sort (proximosObstaculosValidos 5 (Estrada 1, [Nenhum, Carro, Carro, Carro]))
  , "(Estrada 1, [Nenhum, Carro, Carro]) // 5" ~: [Nenhum, Carro] ~=? sort (proximosObstaculosValidos 5 (Estrada 1, [Nenhum, Carro, Carro]))
  , "(Estrada 1, [Carro, Carro]) // 3" ~: [Nenhum] ~=? sort (proximosObstaculosValidos 3 (Estrada 1, [Carro, Carro]))
  , "(Relva, [Arvore, Arvore]) // 3" ~: [Nenhum] ~=? sort (proximosObstaculosValidos 3 (Relva, [Arvore, Arvore]))
  ]

testesT2_estende = TestLabel "#estendeMapa" $ test
  [ "Mapa 01 // 42" ~: True ~=? checkMap (estendeMapa mapa01 42)
  , "Mapa 01 // 5" ~: True ~=? checkMap (estendeMapa mapa01 5)
  , "Mapa 02 // 42" ~: True ~=? checkMap (estendeMapa mapa02 42)
  , "Mapa 02 // 5" ~: True ~=? checkMap (estendeMapa mapa02 5)
  , "Mapa 03 // 42" ~: True ~=? checkMap (estendeMapa mapa03 42)
  , "Mapa 03 // 5" ~: True ~=? checkMap (estendeMapa mapa03 5)
  ]

testsT2 = TestLabel "Tarefa 2" $ test [testesT2_terrenos, testesT2_obstaculos, testesT2_estende]

-------------------------------------------------------------------------------
-- Testes T3                                                                 --
-------------------------------------------------------------------------------

testesT3_move = TestLabel "Move o Jogador" $ test
  [ "Jogo 01 // Move Cima" ~: Jogo (Jogador (0, 1)) mapa01' ~=? animaJogo jogo01 (Move Cima)
  -- , "Vai para tronco à direita" ~: Jogo (Jogador (3, 0)) mapa01' ~=? animaJogo (Jogo (Jogador (1, 0)) mapa01) (Move Direita)
  , "Fica parado em cima de tronco deslizando direita" ~: Jogo (Jogador (2, 0)) mapa01' ~=? animaJogo (Jogo (Jogador (1, 0)) mapa01) Parado
  , "Fica parado em cima de tronco deslizando esquerda" ~: Jogo (Jogador (-1, 1)) mapa01' ~=? animaJogo (Jogo (Jogador (0, 1)) mapa01) Parado
  , "Vai para tronco à esquerda" ~: Jogo (Jogador (0, 2)) mapa01' ~=? animaJogo (Jogo (Jogador (0, 2)) mapa01) (Move Esquerda)
  , "Vai para espaço que vai receber tronco" ~: Jogo (Jogador (2, 1)) mapa01' ~=? animaJogo (Jogo (Jogador (2, 2)) mapa01) (Move Cima)
  -- , "Vai de tronco para tronco" ~: Jogo (Jogador (2, 0)) mapa01' ~=? animaJogo (Jogo (Jogador (1, 1)) mapa01) (Move Cima)
  , "Vai para a esquerda em relva" ~: Jogo (Jogador (0, 8)) mapa03' ~=? animaJogo (Jogo (Jogador (1, 8)) mapa03) (Move Esquerda)
  , "Vai para a direita em relva" ~: Jogo (Jogador (1, 8)) mapa03' ~=? animaJogo (Jogo (Jogador (0, 8)) mapa03) (Move Direita)
  , "Vai para baixo em relva" ~: Jogo (Jogador (1, 8)) mapa03' ~=? animaJogo (Jogo (Jogador (1, 7)) mapa03) (Move Baixo)
  , "Vai para cima em relva" ~: Jogo (Jogador (1, 7)) mapa03' ~=? animaJogo (Jogo (Jogador (1, 8)) mapa03) (Move Cima)
  , "Sai de Rio para Estrada" ~: Jogo (Jogador (1, 3)) mapa03' ~=? animaJogo (Jogo (Jogador (1, 4)) mapa03) (Move Cima)
  , "Sai de Rio para Estrada" ~: Jogo (Jogador (1, 3)) mapa03' ~=? animaJogo (Jogo (Jogador (1, 4)) mapa03) (Move Cima)
  , "Para Cima de Carro" ~: Jogo (Jogador (1, 1)) mapa03' ~=? animaJogo (Jogo (Jogador (1, 2)) mapa03) (Move Cima)
  , "Para debaixo de Carro" ~: Jogo (Jogador (3, 1)) mapa03' ~=? animaJogo (Jogo (Jogador (3, 2)) mapa03) (Move Cima)
  , "Sai de Estrada para Relva" ~: Jogo (Jogador (1, 4)) mapa04' ~=? animaJogo (Jogo (Jogador (1, 5)) mapa04) (Move Cima)
  ]

testesT3_parado = TestLabel "Jogador fica parado" $ test
  [ "Jogo 01 // Move Direita" ~: Jogo jogador01 mapa01' ~=? animaJogo jogo01 (Move Direita)
  , "Jogo 01 // Move Esquerda" ~: Jogo jogador01 mapa01' ~=? animaJogo jogo01 (Move Esquerda)
  , "Jogo 01 // Move Baixo" ~: Jogo jogador01 mapa01' ~=? animaJogo jogo01 (Move Baixo)
  , "Jogo 01 // Parado" ~: Jogo jogador01 mapa01' ~=? animaJogo jogo01 Parado
  -- , "Não pode ir contra Carro à Esquerda" ~: Jogo (Jogador (2, 0)) mapa03' ~=? animaJogo (Jogo (Jogador (2, 0)) mapa03) (Move Esquerda)
  -- , "Não pode ir contra Carro à Direita" ~: Jogo (Jogador (2, 2)) mapa03'' ~=? animaJogo (Jogo (Jogador (2, 2)) mapa03') (Move Direita)
  , "Tenta sair do mapa para cima" ~: Jogo (Jogador (2, 0)) mapa01' ~=? animaJogo (Jogo (Jogador (1, 0)) mapa01) (Move Cima)
  , "Contra arvore à esquerda" ~: Jogo (Jogador (2, 2)) mapa01' ~=? animaJogo (Jogo (Jogador (2, 2)) mapa01) (Move Esquerda)
  , "Tenta sair do mapa pela direita" ~: Jogo (Jogador (2, 2)) mapa01' ~=? animaJogo (Jogo (Jogador (2, 2)) mapa01) (Move Direita)
  ]

testsT3 = TestLabel "Tarefa 3" $ test [testesT3_move, testesT3_parado]

-------------------------------------------------------------------------------
-- Testes T4                                                                 --
-------------------------------------------------------------------------------

testesT4_continua = TestLabel "Situações Válidas" $ test
  [ "Jogo 01 é válido" ~: False ~=? jogoTerminou jogo01
  , "Jogador pode estar em cima de tronco ao andar esquerda" ~: False ~=? jogoTerminou (Jogo (Jogador (0, 1)) mapa01)
  , "Jogador pode estar em cima de tronco ao andar direita" ~: False ~=? jogoTerminou (Jogo (Jogador (2, 0)) mapa01)
  , "Jogador pode estar em cima de tronco" ~: False ~=? jogoTerminou (Jogo (Jogador (1, 1)) mapa01)
  , "Jogador pode estar em estrada" ~: False ~=? jogoTerminou (Jogo (Jogador (2, 0)) mapa03)
  ]

testesT4_terminou = TestLabel "Situações Inválidas" $ test
  [ "Jogo 01' é inválido" ~: True ~=? jogoTerminou jogo01'
  , "Jogador está fora à esquerda" ~: True ~=? jogoTerminou (Jogo (Jogador (-1, 1)) mapa01')
  , "Jogador está fora à direita" ~: True ~=? jogoTerminou (Jogo (Jogador (3, 0)) mapa01')
  , "Jogador está fora à frente" ~: True ~=? jogoTerminou (Jogo (Jogador (2, -1)) mapa01')
  , "Jogador não pode estar em cima de árvore" ~: True ~=? jogoTerminou (Jogo (Jogador (1, 2)) mapa01')
  , "Jogador não pode estar em cima de carro" ~: True ~=? jogoTerminou (Jogo (Jogador (2, 0)) mapa03')
  , "Jogador não pode estar em água" ~: True ~=? jogoTerminou (Jogo (Jogador (1, 4)) mapa03')
  ]

testsT4 = TestLabel "Tarefa 4" $ TestLabel "#jogoTerminou" $ test [testesT4_continua, testesT4_terminou]

-------------------------------------------------------------------------------
-- Main                                                                      --
-------------------------------------------------------------------------------
-- main = runTestTTAndExit $ test [testsT1, testsT2, testsT3, testsT4]

main = do
  createDirectoryIfMissing True "errors/"
  createDirectoryIfMissing True "results/"

  fileT1          <- openFile "errors/T1.txt" WriteMode
  (countT1, out2) <- runTestText (putTextToHandle fileT1 True)
    $ TestList [testsT1]
  -- countT1 <- runTestTT $ TestList [testsT1]
  writeFile "results/T1.txt" (show countT1)

  fileT2          <- openFile "errors/T2.txt" WriteMode
  (countT2, out2) <- runTestText (putTextToHandle fileT2 True)
    $ TestList [testsT2]
  -- countT2 <- runTestTT $ TestList [testsT2]
  writeFile "results/T2.txt" (show countT2)

  fileT3          <- openFile "errors/T3.txt" WriteMode
  (countT3, out3) <- runTestText (putTextToHandle fileT3 True)
    $ TestList [testsT3]
  -- countT3 <- runTestTT $ TestList [testsT3]
  writeFile "results/T3.txt" (show countT3)

  fileT4          <- openFile "errors/T4.txt" WriteMode
  (countT4, out4) <- runTestText (putTextToHandle fileT4 True)
    $ TestList [testsT4]
  -- countT4 <- runTestTT $ TestList [testsT4]
  writeFile "results/T4.txt" (show countT4)

  file         <- openFile "errors/total.txt" WriteMode
  (count, out) <- runTestText (putTextToHandle file True)
    $ TestList [testsT1, testsT2, testsT3, testsT4]
  -- count <- runTestTT $ TestList [testsT1, testsT2, testsT3, testsT4]
  writeFile "results/total.txt" (show count)

  when (failures count > 0) exitFailure
