{- |
Module      : Lib
Description : Define funções para manipulação da posição de um objeto em uma linha.
-}
module Lib
    ( posInicial
    , mostraPos
    , limpaPos
    , movEsq
    , movDir
    , movBaixo
    , identificaMov
    ) where

-- | Representa a posição inicial do objeto na linha.
-- A linha possui um caractere 'S' representando a posição do objeto.
posInicial :: String
posInicial = "|--------------------------S--------------------------|"
              
-- | Exibe a posição atual na saída padrão.
-- Recebe uma string que representa a posição atual e imprime-a no terminal.
mostraPos :: String -> IO()
mostraPos pos = putStrLn pos

-- | Representa uma posição vazia.
-- Uma string vazia é usada para representar a "limpeza" da posição.
limpaPos :: String
limpaPos = ""

-- | Move o objeto uma posição para a direita.
-- Simula o movimento do objeto movendo o caractere 'S' para a direita
-- na string de posição.
movDir :: String -> String
movDir posAtual = "Movendo para a direita: " ++ posAtual

-- | Move o objeto uma posição para a esquerda.
-- Simula o movimento do objeto movendo o caractere 'S' para a esquerda
-- na string de posição.
movEsq :: String -> String
movEsq posAtual = "Movendo para a esquerda: " ++ posAtual

-- | Move o objeto uma posição para baixo.
-- Simula o movimento do objeto para baixo (provavelmente muda de linha).
movBaixo :: String -> String
movBaixo posAtual = "Movendo para baixo: " ++ posAtual

-- | Identifica o comando do usuário e aplica o movimento correspondente.
-- 'a' move para a esquerda, 's' move para baixo, 'd' move para a direita.
-- 'ESC' encerra o jogo, e qualquer outro comando faz o jogo reiniciar.
identificaMov :: Char -> String -> String
identificaMov mov posAtual
    | mov == 'a'  = movEsq posAtual
    | mov == 'd'  = movDir posAtual
    | mov == 's'  = movBaixo posAtual
    | mov == '\ESC' = "Jogo encerrado."  -- ESC encerra o jogo
    | otherwise   = "Comando inválido! Reiniciando o jogo..."  -- Para comandos inválidos
