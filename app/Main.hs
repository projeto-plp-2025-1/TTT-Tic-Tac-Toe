module Main where

import Tabuleiro

main :: IO ()
main = do
    let tabGrande = criarTabuleiroGrande
    putStrLn "=== Tabuleiro Pequeno Cru ==="
    putStrLn $ mostrarTabuleiroPequenoCrua (head (head tabGrande))
    putStrLn "=== Tabuleiro Grande Cru ==="
    putStrLn $ mostrarTabuleiroGrandeCrua tabGrande
