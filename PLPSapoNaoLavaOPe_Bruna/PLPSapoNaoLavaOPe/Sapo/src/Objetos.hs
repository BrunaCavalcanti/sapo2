{- |
Module      : Objetos
Description : Define tipos de dados para representar diferentes objetos do jogo, como Tronco, Regia e Sapo.
-}
module Objetos (Tronco(..), Regia(..), Sapo(..)) where

-- | Tipo para representar uma posição no grid.
-- Representa a posição no grid utilizando dois valores inteiros: a coordenada x e y.
type Position = (Int, Int)

-- | Tipo para representar um Tronco.
-- Cada tronco possui uma posição e uma velocidade (de movimento).
data Tronco = Tronco
  { troncoPosition :: Position  -- Posição do tronco no grid
  , troncoSpeed :: Int         -- Velocidade do tronco, controla a movimentação do tronco
  } deriving (Eq, Show)

-- | Tipo para representar uma Regia (vitória-régia).
-- Cada vitória-régia tem uma posição e uma velocidade (de movimento).
data Regia = Regia
  { regiaPosition :: Position  -- Posição da vitória-régia no grid
  , regiaSpeed :: Int         -- Velocidade da vitória-régia, controla o movimento dela
  } deriving (Eq, Show)

-- | Tipo para representar o Sapo.
-- O sapo tem uma posição no grid e um número de vidas (quantas vezes o sapo pode "morrer").
data Sapo = Sapo
  { sapoPosition :: Position  -- Posição do sapo no grid
  , sapoLives :: Int         -- Número de vidas restantes do sapo
  } deriving (Eq, Show)
