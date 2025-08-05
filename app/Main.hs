module Main where

import System.Process (callCommand)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Info (os)
import Utils.Types
import System.IO (hFlush, stdout)
import Data.Char (toUpper)
import Interface.Arte (clearScreen, exibirInicio)
import Interface.Regras (exibirRegras)
import Interface.Menu (exibirMenu)
import Core.TabuleiroMaior (gameLoop)
import Core.TabuleiroMenor (
    smallBoard1Template, smallBoard2Template, smallBoard3Template,
    smallBoard4Template, smallBoard5Template, smallBoard6Template,
    smallBoard7Template, smallBoard8Template, smallBoard9Template
    )

-- Compatibilidade com Windows e Linux
setUtf8EncodingCompat :: IO ()
setUtf8EncodingCompat =
    if os == "mingw32"
        then callCommand "chcp 65001 > nul"
        else return ()

-- Lista de personagens disponíveis: número, símbolo, nome padrão
personagens :: [(Int, Char, String)]
personagens =
    [ (1, 'X', "Xis")
    , (2, 'O', "Bola")
    , (3, '#', "Cerquinha")
    , (4, '@', "Arroba")
    , (5, '&', "E-Comercial")
    , (6, '%', "Porcento")
    , (7, 'Δ', "Delta")
    , (8, '☺', "Sorriso")
    , (9, '♣', "Trevo")
    ]

-- Mostrar personagens disponíveis
mostrarPersonagens :: [(Int, Char, String)] -> IO ()
mostrarPersonagens ps = do
    putStrLn "Escolha um personagem pelo número:"
    mapM_ (\(i, s, n) -> putStrLn $ show i ++ " -> [" ++ [s] ++ "] " ++ n) ps

-- Pedir escolha de personagem (símbolo + nome)
escolherPersonagem :: [Int] -> IO (Char, String)
escolherPersonagem usados = do
    let disponiveis = filter (\(i, _, _) -> i `notElem` usados) personagens
    mostrarPersonagens disponiveis
    putStr "Digite o número do personagem desejado: "
    hFlush stdout
    input <- getLine
    case reads input :: [(Int, String)] of
        [(num, "")] ->
            case lookupPersonagem num disponiveis of
                Just (s, nomePadrao) -> do
                    putStr $ "Deseja um nome personalizado para o personagem [" ++ [s] ++ "]? (pressione Enter para usar '" ++ nomePadrao ++ "'): "
                    hFlush stdout
                    nomeInput <- getLine
                    let nomeFinal = if null nomeInput then nomePadrao else nomeInput
                    return (s, nomeFinal)
                Nothing -> erro
        _ -> erro
  where
    lookupPersonagem n = fmap (\(_, s, nome) -> (s, nome)) . safeHead . filter (\(i, _, _) -> i == n)
    safeHead [] = Nothing
    safeHead (x:_) = Just x
    erro = do
        putStrLn "Escolha inválida. Tente novamente."
        escolherPersonagem usados

-- Função principal
main :: IO ()
main = do
    setUtf8EncodingCompat
    setLocaleEncoding utf8
    clearScreen
    opcao <- exibirMenu
    case opcao of
        "1" -> iniciarJogo
        "2" -> exibirRegras >> main
        "3" -> sairDoJogo
        _   -> main

-- Início do jogo com personagens personalizados
iniciarJogo :: IO ()
iniciarJogo = do
    putStrLn "JOGADOR 1"
    (symbol1, name1) <- escolherPersonagem []
    putStrLn $ "Jogador 1 escolheu: [" ++ [symbol1] ++ "] " ++ name1

    putStrLn "\nJOGADOR 2"
    (symbol2, name2) <- escolherPersonagem [getId symbol1]
    if map toUpper name2 == map toUpper name1
        then do
            putStrLn "Nome já utilizado. Escolha outro nome para o Jogador 2."
            iniciarJogo
        else do
            putStrLn $ "Jogador 2 escolheu: [" ++ [symbol2] ++ "] " ++ name2

            putStrLn "\nPressione Enter para começar..."
            _ <- getLine

            let initialBoard = -- Template do tabuleiro maior
                        [
                        "    1    2    3     4    5    6     7    8    9",
                        "                                                ",
                        "       │    │    ║    │    │    ║    │    │      A",
                        "   ────┼────┼────╢────┼────┼────╢────┼────┼──── ",
                        "       │    │    ║    │    │    ║    │    │      B",
                        "   ────┼────┼────╢────┼────┼────╢────┼────┼──── ",
                        "       │    │    ║    │    │    ║    │    │      C",
                        "     QUADRANTE 1 ║  QUADRANTE 2 ║  QUADRANTE 3  ",
                        "   ══════════════╬══════════════╬══════════════ ",
                        "       │    │    ║    │    │    ║    │    │      D",
                        "   ────┼────┼────╢────┼────┼────╢────┼────┼──── ",
                        "       │    │    ║    │    │    ║    │    │      E",
                        "   ────┼────┼────╢────┼────┼────╢────┼────┼──── ",
                        "       │    │    ║    │    │    ║    │    │      F",
                        "     QUADRANTE 4 ║  QUADRANTE 5 ║  QUADRANTE 6  ",
                        "   ══════════════╬══════════════╬══════════════ ",
                        "       │    │    ║    │    │    ║    │    │      G",
                        "   ────┼────┼────╢────┼────┼────╢────┼────┼──── ",
                        "       │    │    ║    │    │    ║    │    │      H",
                        "   ────┼────┼────╢────┼────┼────╢────┼────┼──── ",
                        "       │    │    ║    │    │    ║    │    │      I",
                        "     QUADRANTE 7 ║  QUADRANTE 8 ║  QUADRANTE 9  ",
                        "                                                "
                        ]

            let initialSmallBoards =
                    [ smallBoard1Template, smallBoard2Template, smallBoard3Template
                    , smallBoard4Template, smallBoard5Template, smallBoard6Template
                    , smallBoard7Template, smallBoard8Template, smallBoard9Template
                    ]

            gameLoop initialBoard initialSmallBoards symbol1 symbol2 symbol1 Nothing name1 name2

-- Função auxiliar: obter o ID de um símbolo (para evitar repetição)
getId :: Char -> Int
getId c = case lookup c (map (\(i, s, _) -> (s, i)) personagens) of
    Just i -> i
    Nothing -> -1

-- Sair do jogo
sairDoJogo :: IO ()
sairDoJogo = putStrLn "Saindo do jogo. Até mais!"
