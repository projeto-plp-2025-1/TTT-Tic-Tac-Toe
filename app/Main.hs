module Main where

import Interface.TabuleiroMaior (gameLoop)
import Types
import Arte (clearScreen)
import System.IO (hFlush, stdout)
import Interface.TabuleiroMenor (smallBoard1Template, smallBoard2Template, smallBoard3Template,
                                smallBoard4Template, smallBoard5Template, smallBoard6Template,
                                smallBoard7Template, smallBoard8Template, smallBoard9Template)

main :: IO ()
main = do
    clearScreen
    putStrLn "Welcome to Ultimate Tic-Tac-Toe!"
    putStrLn "Obrigado por jogar!"
    putStr "Pressione Enter para começar..."
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

    -- A LISTA inicial com os 9 templates, garantindo que você tenha o estado de cada um.
    let initialSmallBoards = [
            smallBoard1Template, smallBoard2Template, smallBoard3Template,
            smallBoard4Template, smallBoard5Template, smallBoard6Template,
            smallBoard7Template, smallBoard8Template, smallBoard9Template
            ]
    
    -- Chama o loop principal, passando o estado completo do jogo
    gameLoop initialBoard initialSmallBoards 'X'