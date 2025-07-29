module Main where

import Tabuleiro
import Arte

main :: IO ()
main = do
    exibirInicio
    let tabGrande = criarTabuleiroGrande
    putStrLn "=== Tabuleiro Pequeno Cru ==="
    putStrLn $ mostrarTabuleiroPequenoCrua (head (head tabGrande))
    putStrLn "=== Tabuleiro Grande Cru ==="
    putStrLn $ mostrarTabuleiroGrandeCrua tabGrande
