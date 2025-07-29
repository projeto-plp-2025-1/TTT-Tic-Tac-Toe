module Tabuleiro (
    Celula(..),
    TabuleiroPequeno,
    TabuleiroGrande,
    criarTabuleiroPequeno,
    criarTabuleiroGrande,
    mostrarTabuleiroPequenoCrua,
    mostrarTabuleiroGrandeCrua
) where

import Data.List (intercalate)

-- Representa o valor de uma célula
data Celula = Vazia | X | O deriving (Eq)

instance Show Celula where
    show Vazia = "0"
    show X     = "X"
    show O     = "O"

-- Um tabuleiro pequeno (3x3) é uma matriz de células
type TabuleiroPequeno = [[Celula]]

-- Um tabuleiro grande (3x3) é uma matriz de tabuleiros pequenos
type TabuleiroGrande = [[TabuleiroPequeno]]

-- Cria um tabuleiro pequeno vazio (3x3)
criarTabuleiroPequeno :: TabuleiroPequeno
criarTabuleiroPequeno = replicate 3 (replicate 3 Vazia)

-- Cria um tabuleiro grande vazio (3x3 de tabuleiros pequenos)
criarTabuleiroGrande :: TabuleiroGrande
criarTabuleiroGrande = replicate 3 (replicate 3 criarTabuleiroPequeno)

-- Mostra célula cru (0 para vazia)
mostrarCelulaCrua :: Celula -> String
mostrarCelulaCrua Vazia = "0"
mostrarCelulaCrua X     = "X"
mostrarCelulaCrua O     = "O"

-- Mostra linha cru do tabuleiro pequeno (ex: "0 0 X")
mostrarLinhaCrua :: [Celula] -> String
mostrarLinhaCrua linha = intercalate " " (map mostrarCelulaCrua linha)

-- Mostra tabuleiro pequeno cru, linha por linha
mostrarTabuleiroPequenoCrua :: TabuleiroPequeno -> String
mostrarTabuleiroPequenoCrua tab =
    unlines $ map mostrarLinhaCrua tab

mostrarTabuleiroGrandeCrua :: TabuleiroGrande -> String
mostrarTabuleiroGrandeCrua tabuleiroGrande =
    let
        juntarLinhasLinhaGrande :: [TabuleiroPequeno] -> [String]
        juntarLinhasLinhaGrande linhaTabs =
            let linhasTabs = map (map mostrarLinhaCrua) linhaTabs
                combinarLinha n = intercalate " || " [ linhas !! n | linhas <- linhasTabs ]
            in [combinarLinha 0, combinarLinha 1, combinarLinha 2]

        separadorLinha = replicate 6 '-' ++ "++" ++ replicate 7 '-' ++ "++" ++ replicate 6 '-'

        grupos = map juntarLinhasLinhaGrande tabuleiroGrande
        linhasCompletas = intercalate [separadorLinha] grupos
    in unlines linhasCompletas


