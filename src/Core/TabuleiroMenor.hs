module Core.TabuleiroMenor where

import Utils.Types
import Interface.Arte (clearScreen)
import Data.Char (toUpper)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Utils.VerificacaoVitoria (verificarVitoria)

smallBoard1Template :: [String]
smallBoard1Template = [
    "    1    2    3      ",
    "  ╔══════════════╗   ",
    "  ║    │    │    ║ A ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ B ",
    "  ║────┼────┼────╢   ",
    "  ║    │    │    ║ C ",
    "  ║  QUADRANTE 1 ║   ",
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
    "  ║  QUADRANTE 2 ║   ",
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
    "  ║  QUADRANTE 3 ║   ",
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
    "  ║  QUADRANTE 4 ║   ",
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
    "  ║  QUADRANTE 5 ║   ",
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
    "  ║  QUADRANTE 6 ║   ",
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
    "  ║  QUADRANTE 7 ║   ",
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
    "  ║  QUADRANTE 8 ║   ",
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
    "  ║  QUADRANTE 9 ║   ",
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

tryMoveSmall :: [String] -> Char -> Int -> Char -> Maybe ([String], Bool)
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
                        venceu = verificarVitoria newBoard player
                    in Just (newBoard, venceu)
                else Nothing
        _ -> Nothing


gameLoopSmall :: [String] -> Char -> IO (Maybe [String])
gameLoopSmall board player = do
    clearScreen
    putStrLn ""
    putStrLn (unlines board)
    putStrLn $ "Turno do Jogador: " ++ [player]
    putStrLn "(Tabuleiro Menor)"

    putStrLn "Aperte Enter para prosseguir, ou 'Q' para retornar ao tabuleiro maior: "
    opcao <- getLine
    if map toUpper opcao == "Q"
        then do
            putStrLn "Retornando ao tabuleiro maior..."
            return Nothing
        else do
            putStr "Digite a linha: "
            hFlush stdout
            lineInput <- getLine

            putStr "Digite a coluna: "
            hFlush stdout
            colInput <- getLine

            let maybeCol = readMaybe colInput :: Maybe Int

            case (lineInput, maybeCol) of
                (l:_, Just c) -> do
                    case tryMoveSmall board (toUpper l) c player of
                        Just (newBoard, venceu) -> do
                            putStrLn "Jogada no tabuleiro menor realizada!"
                            if venceu
                                then putStrLn $ "Vitória do jogador " ++ [player] ++ " no tabuleiro menor!"
                                else return ()
                            putStrLn "Pressione Enter para retornar ao tabuleiro maior..."
                            _ <- getLine
                            return (Just newBoard)
                        Nothing -> do
                            putStrLn "--- JOGADA INVÁLIDA! Tente novamente. ---"
                            gameLoopSmall board player
                _ -> do
                    putStrLn "--- ENTRADA INVÁLIDA! Use uma letra e número válidos. ---"
                    gameLoopSmall board player
