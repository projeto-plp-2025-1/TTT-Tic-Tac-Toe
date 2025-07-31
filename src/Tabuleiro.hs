module Tabuleiro where

import Types
import Arte (clearScreen)
import System.IO (readFile)

-- Converte uma célula do tabuleiro para um caractere para exibição
cellToChar :: Cell -> Char
cellToChar Nothing  = '.'
cellToChar (Just p) = head (show p)

-- Preenche o template do tabuleiro com os dados atuais do jogo
popularTemplate :: String -> [Char] -> String
popularTemplate [] _ = []
popularTemplate template [] = template
popularTemplate (t:ts) (d:ds)
    | t == '_'  = d : popularTemplate ts ds
    | otherwise = t : popularTemplate ts (d:ds)

-- Desenha o tabuleiro na tela
desenharTabuleiro :: GameState -> IO ()
desenharTabuleiro gameState = do
    template <- readFile "src/board_template.txt"
    let todasAsCelulas = concatMap concat (concat (board gameState))
    let celulasEmChar = map cellToChar todasAsCelulas
    let tabuleiroPreenchido = popularTemplate template celulasEmChar
    clearScreen
    putStrLn tabuleiroPreenchido
