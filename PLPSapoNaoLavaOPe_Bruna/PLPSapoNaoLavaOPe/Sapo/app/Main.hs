module Main where

import Data.List
import Data.Maybe
import Graphics.Gloss (Color, makeColor, text, scale, translate)
import Graphics.Gloss.Interface.Pure.Game
import Grid (Cell (..), GameState (..), Grid, getCell, updateCell)
import Objetos (Regia (..), Sapo (..), Tronco (..))

-- Definindo as cores específicas da biblioteca Gloss
-- Essas cores são usadas para representar diferentes tipos de células no grid (por exemplo, terra, água, troncos, etc.)

--Cores especificas da biblioteca Gloss
brown :: Color
brown = makeColor 0.65 0.16 0.16 1.0  -- Cor marrom para representar a Terra

lightBlue :: Color
lightBlue = makeColor 0.53 0.81 0.98 1.0  -- Cor azul claro para representar a Água

darkBrown :: Color
darkBrown = makeColor 0.36 0.25 0.20 1.0  -- Cor marrom escuro para representar os Troncos

lightGreen :: Color
lightGreen = makeColor 0.56 0.93 0.56 1.0  -- Cor verde claro para representar as Vitórias-régias

darkGreen :: Color
darkGreen = makeColor 0.0 0.5 0.0 1.0  -- Cor verde escuro para representar o Sapo

-- Estado inicial do jogo
-- A estrutura 'GameState' armazena o grid (tabuleiro) e o tempo desde o último movimento.
-- O grid é composto por uma lista de listas de células, e o tempo é utilizado para controlar a atualização dos objetos no jogo.
initialState :: GameState
initialState = GameState
  { grid = 
      [ [Terra, Terra, Terra, Terra, Terra, Terra, Terra],  -- Linha 0 (terra)
        [Water, Water, RegiaCell (Regia (6,3) 2), Water, Water, Water, Water],  -- Linha 1 (água e vitórias-régias)
        [Water, Water, Water, TroncoCell (Tronco (0,5) 1), TroncoCell (Tronco (0,5) 1), Water, Water],  -- Linha 2 (água e troncos)
        [Water, Water, Water, RegiaCell (Regia (6,3) 2), Water, Water, Water],  -- Linha 3 (água e vitórias-régias)
        [Water, Water, Water, TroncoCell (Tronco (0,5) 1), TroncoCell (Tronco (0,5) 1), Water, Water],  -- Linha 4 (água e troncos)
        [RegiaCell (Regia (6,3) 2), Water, Water, Water, Water, Water, Water],  -- Linha 5 (vitórias-régias)
        [Terra, Terra, Terra, SapoCell (Sapo (3,6) 3), Terra, Terra, Terra]   -- Linha 6 (terra e sapo)
      ],
    timeSinceLastMove = 0.1  -- Tempo desde o último movimento
  }

-- Função de renderização: converte o estado do jogo em uma representação gráfica (imagem)
render :: GameState -> Picture
render state =
  pictures $  -- Cria uma lista de imagens a partir do grid
    [ translate (fromIntegral x * 50 - 100) (fromIntegral y * 50 - 100) (cellToPicture cell)
      | (y, row) <- zip [0 ..] (grid state),    -- Itera sobre as linhas e colunas do grid
        (x, cell) <- zip [0 ..] row
    ]
    ++ victory  -- Verifica se o jogo foi vencido e exibe a mensagem de vitória
  where
    -- Função para converter uma célula em uma figura (Picture)
    cellToPicture :: Cell -> Picture
    cellToPicture Empty = color white (rectangleWire 50 50)  -- Célula vazia representada por um retângulo branco
    cellToPicture Terra = color brown (rectangleSolid 50 50)  -- Terra representada por um retângulo marrom
    cellToPicture Water = color lightBlue (rectangleSolid 50 50)  -- Água representada por um retângulo azul claro
    cellToPicture (TroncoCell _) = color darkBrown (rectangleSolid 50 50)  -- Tronco representado por um retângulo marrom escuro
    cellToPicture (RegiaCell _) = color lightGreen (rectangleSolid 50 50)  -- Vitória-régia representada por um retângulo verde claro
    cellToPicture (SapoCell _) = color darkGreen (circleSolid 20)  -- Sapo representado por um círculo verde escuro

    -- Se o sapo estiver na linha 0, exibe uma mensagem de vitória
    victory =
      case findSapo (grid state) of
        Just (_, 0) -> [translate (-80) 0 (scale 0.2 0.2 (text "Parabéns! Você não lavou o pé!"))]
        _ -> []  -- Se o sapo não chegou à linha 0, não exibe a mensagem

-- Função de atualização: atualiza o estado do jogo a cada "frame" com base no tempo
update :: Float -> GameState -> GameState
update deltaTime state =
  let newTime = timeSinceLastMove state + deltaTime  -- Atualiza o tempo
      moveInterval = 0.6  -- Intervalo de tempo entre os movimentos (em segundos)
  in if newTime >= moveInterval
     then state { grid = moveObjects (grid state), timeSinceLastMove = 0.0 }  -- Move objetos quando o intervalo de tempo é atingido
     else state { timeSinceLastMove = newTime }  -- Caso contrário, apenas atualiza o tempo

-- Função para mover troncos e vitórias-régias de forma independente
moveObjects :: Grid -> Grid
moveObjects = zipWith moveRow [0 ..]  -- Para cada linha do grid, move os objetos de acordo com o índice da linha
  where
    initialSapoRowIndex = 6  -- Linha inicial do sapo
    finalSapoRowIndex = 0    -- Linha final (onde o sapo vence)
    moveRow :: Int -> [Cell] -> [Cell]
    moveRow rowIndex row
      | rowIndex `elem` [initialSapoRowIndex, finalSapoRowIndex] = row  -- As linhas com o sapo não se movem
      | rowIndex `elem` [2, 4] = moveBy 1 row  -- Troncos se movem 1 posição nas linhas 2 e 4
      | rowIndex `elem` [1, 3, 5] = moveBy 2 row  -- Vitórias-régias se movem 2 posições nas linhas 1, 3 e 5
      | otherwise = row  -- Outras linhas não se movem
      where
        -- Função para mover a lista de células ciclicamente
        moveBy n list = drop n list ++ take n list

-- Função de entrada (movimento do sapo)
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state =
  state {grid = moveSapo (grid state) (0, 1)}  -- Mover o sapo para cima
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state =
  state {grid = moveSapo (grid state) (0, -1)}  -- Mover o sapo para baixo
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state =
  state {grid = moveSapo (grid state) (-1, 0)}  -- Mover o sapo para a esquerda
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state =
  state {grid = moveSapo (grid state) (1, 0)}  -- Mover o sapo para a direita
handleInput _ state = state  -- Ignora outros eventos de teclado

-- Posição inicial do sapo
initialSapoPos :: (Int, Int)
initialSapoPos = (3, 6)

-- Função para mover o sapo
moveSapo :: Grid -> (Int, Int) -> Grid
moveSapo grid (dx, dy) =
  case findSapo grid of
    Just (x, y) -> 
      let newX = x + dx  -- Nova posição X
          newY = y + dy  -- Nova posição Y
          maxX = length (head grid) - 1  -- Limite máximo de X (largura do grid)
          maxY = length grid - 1  -- Limite máximo de Y (altura do grid)
          newPos = (newX, newY)
      in if newX >= 0 && newX <= maxX && newY >= 0 && newY <= maxY  -- Verifica se a nova posição é válida
           then case getCell grid newPos of
             Just Water ->  -- Se a nova posição for água, o sapo retorna para a posição inicial
               case updateCell grid (x, y) Empty of
                 Just gridWithoutSapo -> 
                   fromMaybe grid 
                     (updateCell gridWithoutSapo initialSapoPos (SapoCell (Sapo initialSapoPos 3)))
                 Nothing -> grid
             _ ->  -- Caso contrário, o sapo se move normalmente
               case updateCell grid (x, y) Empty of
                 Just gridWithoutSapo -> 
                   fromMaybe grid
                     (updateCell gridWithoutSapo newPos (SapoCell (Sapo (newX, newY) 3)))
                 Nothing -> grid
           else grid  -- Não move se a nova posição estiver fora do grid
    Nothing -> grid  -- Se não encontrar o sapo, retorna o grid original

-- Função para encontrar a posição do sapo no grid
findSapo :: Grid -> Maybe (Int, Int)
findSapo grid =
  let indexedRows = zip [0 ..] grid
   in foldl
        ( \acc (y, row) ->
            case findIndex
              ( \cell -> case cell of
                  SapoCell (Sapo (posX, posY) _) -> True  -- Verifica se a célula contém o sapo
                  _ -> False
              )
              row of
              Just x -> Just (x, y)  -- Retorna a posição (x, y) do sapo
              Nothing -> acc  -- Continua procurando
        )
        Nothing  -- Se o sapo não for encontrado, retorna Nothing

-- Função principal que inicializa e roda o jogo
main :: IO ()
main = play
    (InWindow "OSapoNaoLavaOPe" (400, 400) (100, 100))  -- Janela do jogo
    white  -- Cor de fundo da janela
    30  -- Taxa de atualização (FPS)
    initialState  -- Estado inicial do jogo
    render  -- Função para renderizar o estado do jogo
    handleInput  -- Função para lidar com a entrada do jogador (movimento do sapo)
    update  -- Função para atualizar o estado do jogo
