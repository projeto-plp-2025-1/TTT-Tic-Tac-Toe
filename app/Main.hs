module Main where

import System.Process (callCommand)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Info (os)
import Utils.Types
import System.IO (hFlush, stdout)
import Interface.Arte (clearScreen, exibirInicio)
import Interface.Regras (exibirRegras)
import Interface.Menu (exibirMenu)
import Core.TabuleiroMaior (gameLoop)
import Core.TabuleiroMenor (smallBoard1Template, smallBoard2Template, smallBoard3Template,
                                smallBoard4Template, smallBoard5Template, smallBoard6Template,
                                smallBoard7Template, smallBoard8Template, smallBoard9Template)

-- Compatibilidade com Windows e Linux
setUtf8EncodingCompat :: IO ()
setUtf8EncodingCompat =
    if os == "mingw32"
        then callCommand "chcp 65001 > nul"
        else return ()

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
        _   -> main -- caso opção inválida, volta ao menu

-- Função para pedir símbolo, garantindo que não seja espaço e não seja o já escolhido
askPlayerSymbol :: String -> [Char] -> IO Char
askPlayerSymbol prompt taken = do
    putStr (prompt ++ " (não pode ser espaço um espaço vazio e nem nenhum destes -> " ++ taken ++ "): ")
    hFlush stdout
    input <- getLine
    case input of
        [c] | c /= ' ' && c `notElem` taken -> return c
        _ -> do
            putStrLn "Símbolo inválido, tente novamente."
            askPlayerSymbol prompt taken

iniciarJogo :: IO ()
iniciarJogo = do
    putStrLn "Escolha o símbolo do Jogador 1:"
    player1Symbol <- askPlayerSymbol "Símbolo do Jogador 1" []

    putStrLn "Escolha o símbolo do Jogador 2:"
    player2Symbol <- askPlayerSymbol "Símbolo do Jogador 2" [player1Symbol]

    putStrLn $ "Jogador 1 usará: " ++ [player1Symbol]
    putStrLn $ "Jogador 2 usará: " ++ [player2Symbol]

    putStrLn "Pressione Enter para começar..."
    hFlush stdout
    _ <- getLine

    let initialBoard = -- Template do tabuleiro maior
            [
            "    1    2    3     4    5    6     7    8    9",
            "  ╔══════════════╦══════════════╦══════════════╗",
            "  ║    │    │    ║    │    │    ║    │    │    ║ A",
            "  ║────┼────┼────╢────┼────┼────╢────┼────┼────╢",
            "  ║    │    │    ║    │    │    ║    │    │    ║ B",
            "  ║────┼────┼────╢────┼────┼────╢────┼────┼────╢",
            "  ║    │    │    ║    │    │    ║    │    │    ║ C",
            "  ╠══════════════╬══════════════╬══════════════╣",
            "  ║    │    │    ║    │    │    ║    │    │    ║ D",
            "  ║────┼────┼────╢────┼────┼────╢────┼────┼────╢",
            "  ║    │    │    ║    │    │    ║    │    │    ║ E",
            "  ║────┼────┼────╢────┼────┼────╢────┼────┼────╢",
            "  ║    │    │    ║    │    │    ║    │    │    ║ F",
            "  ╠══════════════╬══════════════╬══════════════╣",
            "  ║    │    │    ║    │    │    ║    │    │    ║ G",
            "  ║────┼────┼────╢────┼────┼────╢────┼────┼────╢",
            "  ║    │    │    ║    │    │    ║    │    │    ║ H",
            "  ║────┼────┼────╢────┼────┼────╢────┼────┼────╢",
            "  ║    │    │    ║    │    │    ║    │    │    ║ I",
            "  ╚══════════════╩══════════════╩══════════════╝"
            ]

    let initialSmallBoards = [
            smallBoard1Template, smallBoard2Template, smallBoard3Template,
            smallBoard4Template, smallBoard5Template, smallBoard6Template,
            smallBoard7Template, smallBoard8Template, smallBoard9Template
            ]

    gameLoop initialBoard initialSmallBoards player1Symbol player2Symbol player1Symbol Nothing

sairDoJogo :: IO ()
sairDoJogo = do
    putStrLn "Saindo do jogo. Até mais!"
