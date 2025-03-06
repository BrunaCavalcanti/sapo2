{- |
Module      : Grid
Description : Define a estrutura da grade do jogo, incluindo células, estado do jogo e funções para manipulação da grade.
-}
module Grid (Cell(..), Grid, GameState(..), getCell, updateCell) where

import Objetos (Tronco, Regia, Sapo)

-- | Define os diferentes tipos de células que podem existir no grid do jogo.
-- 'deriving Eq' permite comparar células usando '==' e '/='.
-- 'deriving Show' permite converter células em strings para facilitar a depuração e exibição no terminal.
data Cell
  = Empty            -- ^ Célula vazia, sem objetos.
  | Water           -- ^ Célula representando água, onde o sapo pode se afogar.
  | TroncoCell Tronco -- ^ Célula contendo um tronco, onde o sapo pode se mover.
  | RegiaCell Regia   -- ^ Célula contendo uma vitória-régia (planta), que pode ser o objetivo do sapo.
  | SapoCell Sapo     -- ^ Célula contendo um sapo, o personagem controlado pelo jogador.
  | Terra            -- ^ Célula representando terra, onde o sapo pode se mover.
  deriving (Eq, Show)

-- | O grid é representado por uma matriz de células.
-- O tipo 'Grid' é uma lista de listas, representando a tabela de células (linha por linha).
type Grid = [[Cell]]

-- | Representa o estado do jogo.
-- O estado do jogo inclui o grid atual e o tempo desde o último movimento.
data GameState = GameState
  { grid :: Grid                 -- ^ O grid atual do jogo.
  , timeSinceLastMove :: Float   -- ^ Tempo decorrido desde o último movimento.
  } deriving (Eq, Show)

-- | Obtém uma célula do grid de forma segura.
-- Recebe o grid e as coordenadas (x, y), retornando 'Just Cell' se as coordenadas estiverem dentro dos limites do grid.
-- Caso contrário, retorna 'Nothing', indicando que a célula não é válida.
getCell :: Grid -> (Int, Int) -> Maybe Cell
getCell g (x, y)
  -- Verifica se as coordenadas (x, y) estão dentro dos limites do grid
  | y >= 0 && y < length g && x >= 0 && x < length (g !! y) = Just (g !! y !! x)
  | otherwise = Nothing  -- Se as coordenadas forem inválidas, retorna 'Nothing'

-- | Atualiza uma célula do grid, se as coordenadas forem válidas.
-- Recebe o grid, as coordenadas (x, y) e a nova célula que deve ser colocada na posição (x, y).
-- Retorna um novo grid atualizado dentro de 'Just', ou 'Nothing' se as coordenadas forem inválidas.
updateCell :: Grid -> (Int, Int) -> Cell -> Maybe Grid
updateCell g (x, y) newCell
  -- Verifica se as coordenadas (x, y) estão dentro dos limites do grid
  | y >= 0 && y < length g && x >= 0 && x < length (g !! y) =
      Just (
        take y g ++                           -- Mantém as linhas antes da linha 'y'
        [take x (g !! y) ++ [newCell] ++ drop (x + 1) (g !! y)] ++ -- Substitui a célula na linha 'y' pela nova célula
        drop (y + 1) g                         -- Mantém as linhas após a linha 'y'
      )
  | otherwise = Nothing  -- Se as coordenadas forem inválidas, retorna 'Nothing'
