module Main where

import Utils.Types
import System.IO (hFlush, stdout)
import Interface.Arte (clearScreen, exibirInicio)
import Interface.Regras (exibirRegras)
import Interface.Menu (exibirMenu)
import Core.TabuleiroMaior (gameLoop)
import Core.TabuleiroMenor (smallBoard1Template, smallBoard2Template, smallBoard3Template,
                                smallBoard4Template, smallBoard5Template, smallBoard6Template,
                                smallBoard7Template, smallBoard8Template, smallBoard9Template)

main :: IO ()
main = do
    clearScreen
    opcao <- exibirMenu
    case opcao of
        "1" -> iniciarJogo
        "2" -> exibirRegras >> main
        "3" -> sairDoJogo
        _   -> main -- caso opção inválida, volta ao menu

iniciarJogo :: IO ()
iniciarJogo = do
    putStrLn "Obrigado por jogar!"
    putStr "Pressione Enter para começar..."
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

    gameLoop initialBoard initialSmallBoards 'X'

sairDoJogo :: IO ()
sairDoJogo = do
    putStrLn "Saindo do jogo. Até mais!"
