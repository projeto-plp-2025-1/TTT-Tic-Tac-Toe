-- Mantido com pequenas alterações para focar no fluxo de retorno
module Interface.TabuleiroMenor where

import Types
import Arte (clearScreen)
import Data.Char (toUpper)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

smallBoard1Template :: [String]
smallBoard1Template = [
    "    1    2    3      ",
    "  ╔══════════════╗   ",
    "  ║    │    │    ║ A ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ B ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ C ",
    "  ╚══════════════╝   "
    ]

smallBoard2Template :: [String]
smallBoard2Template = [
    "    4    5    6      ",
    "  ╔══════════════╗   ",
    "  ║    │    │    ║ A ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ B ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ C ",
    "  ╚══════════════╝   "
    ]

smallBoard3Template :: [String]
smallBoard3Template = [
    "    7    8    9      ",
    "  ╔══════════════╗   ",
    "  ║    │    │    ║ A ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ B ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ C ",
    "  ╚══════════════╝   "
    ]

smallBoard4Template :: [String]
smallBoard4Template = [
    "    1    2    3      ",
    "  ╔══════════════╗   ",
    "  ║    │    │    ║ D ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ E ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ F ",
    "  ╚══════════════╝   "
    ]

smallBoard5Template :: [String]
smallBoard5Template = [
    "    4    5    6      ",
    "  ╔══════════════╗   ",
    "  ║    │    │    ║ D ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ E ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ F ",
    "  ╚══════════════╝   "
    ]

smallBoard6Template :: [String]
smallBoard6Template = [
    "    7    8    9      ",
    "  ╔══════════════╗   ",
    "  ║    │    │    ║ D ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ E ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ F ",
    "  ╚══════════════╝   "
    ]

smallBoard7Template :: [String]
smallBoard7Template = [
    "    1    2    3      ",
    "  ╔══════════════╗   ",
    "  ║    │    │    ║ G ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ H ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ I ",
    "  ╚══════════════╝   "
    ]

smallBoard8Template :: [String]
smallBoard8Template = [
    "    4    5    6      ",
    "  ╔══════════════╗   ",
    "  ║    │    │    ║ G ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ H ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ I ",
    "  ╚══════════════╝   "
    ]

smallBoard9Template :: [String]
smallBoard9Template = [
    "    7    8    9      ",
    "  ╔══════════════╗   ",
    "  ║    │    │    ║ G ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ H ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ I ",
    "  ╚══════════════╝   "
    ]

smallBoardTemplate1 :: [String]
smallBoardTemplate1 = smallBoard1Template 
smallBoardTemplate2 :: [String]
smallBoardTemplate2 = smallBoard2Template 
smallBoardTemplate3 :: [String]
smallBoardTemplate3 = smallBoard3Template
smallBoardTemplate4 :: [String]
smallBoardTemplate4 = smallBoard4Template
smallBoardTemplate5 :: [String]
smallBoardTemplate5 = smallBoard5Template
smallBoardTemplate6 :: [String]
smallBoardTemplate6 = smallBoard6Template
smallBoardTemplate7 :: [String]
smallBoardTemplate7 = smallBoard7Template
smallBoardTemplate8 :: [String]
smallBoardTemplate8 = smallBoard8Template
smallBoardTemplate9 :: [String]
smallBoardTemplate9 = smallBoard9Template

getRowIndex :: Char -> Maybe Int
getRowIndex r = case toUpper r of
    'A' -> Just 2;
    'D' -> Just 2;
    'G' -> Just 2;
    'B' -> Just 4;
    'E' -> Just 4;
    'H' -> Just 4;
    'C' -> Just 6;
    'F' -> Just 6;
    'I' -> Just 6;
      _   -> Nothing

getColIndex :: Int -> Maybe Int
getColIndex c = case c of
    1 -> Just 5;
    4 -> Just 5;
    7 -> Just 5;
    2 -> Just 10;
    5 -> Just 10;
    8 -> Just 10;
    3 -> Just 14;
    6 -> Just 14;
    9 -> Just 14;
      _   -> Nothing


replaceAtIndex :: Int -> Char -> String -> String
replaceAtIndex i char str = take i str ++ [char] ++ drop (i + 1) str

tryMoveSmall :: [String] -> Char -> Int -> Char -> Maybe [String]
tryMoveSmall board line col player =
    case (getRowIndex line, getColIndex col) of
        (Just lineIndex, Just colIndex) ->
            let
                targetLine = board !! lineIndex
                isCellEmpty = (targetLine !! colIndex) == ' '
            in
                if isCellEmpty then
                    let
                        updatedLine = replaceAtIndex colIndex player targetLine
                        newBoard = take lineIndex board ++ [updatedLine] ++ drop (lineIndex + 1) board
                    in
                        Just newBoard
                else
                    Nothing
        _ -> Nothing

-- O loop do tabuleiro menor retorna o estado do tabuleiro.
gameLoopSmall :: [String] -> Char -> IO [String]
gameLoopSmall board player = do
    clearScreen
    putStrLn ""
    putStrLn (unlines board)
    putStrLn $ "Turno do Jogador: " ++ [player]
    putStrLn "(Tabuleiro Menor)"

    putStr "Digite a linha (A-C): "
    hFlush stdout
    lineInput <- getLine
    
    putStr "Digite a coluna (1-3): "
    hFlush stdout
    colInput <- getLine

    let maybeCol = readMaybe colInput :: Maybe Int

    case (lineInput, maybeCol) of
        (l:_, Just c) -> do
            case tryMoveSmall board (toUpper l) c player of
                Just newBoard -> do
                    putStrLn "Jogada no tabuleiro menor realizada!"
                    putStrLn "Pressione Enter para retornar ao tabuleiro maior..."
                    _ <- getLine
                    return newBoard -- Retorna o tabuleiro atualizado e encerra este loop
                Nothing -> do
                    putStrLn "--- JOGADA INVÁLIDA! Tente novamente. ---"
                    gameLoopSmall board player
        _ -> do
            putStrLn "--- ENTRADA INVÁLIDA! Use uma letra de A-C e um número de 1-3. ---"
            gameLoopSmall board player