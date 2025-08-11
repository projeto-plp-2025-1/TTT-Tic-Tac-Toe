module Core.TabuleiroMenor where

import Interface.Arte (clearScreen)
import Data.Char (toUpper)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar ( isEmptyMVar, newEmptyMVar, putMVar, MVar )
import Data.Time.Clock ( diffUTCTime, getCurrentTime )
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


gameLoopSmall :: [String] -> Char -> [String] -> IO (Maybe [String])
gameLoopSmall board player bigBoard = do
    startTime <- getCurrentTime
    timeUpVar <- newEmptyMVar

    -- Timer de 2 minutos (120 segundos)
    _ <- forkIO $ do
        threadDelay (2 * 60 * 1000000)
        putMVar timeUpVar ()

    let loop b = do
            clearScreen
            currentTime <- getCurrentTime
            let secondsElapsed = round $ realToFrac $ diffUTCTime currentTime startTime :: Int
            let secondsRemaining = max 0 (120 - secondsElapsed)

            -- Exibe tempo restante
            putStrLn $ "\x23F3 Tempo restante: " ++ show secondsRemaining ++ " segundos"
            putStrLn (unlines b)
            putStrLn $ "Turno do Jogador: " ++ [player] ++ " (Tabuleiro Menor)"
            putStrLn "Aperte Enter para jogar, ou 'V' para voltar ao tabuleiro maior:"
            opcao <- getLine
            if map toUpper opcao == "V"
                then do
                    clearScreen
                    putStrLn "Visualizando tabuleiro maior:"
                    putStrLn ""
                    putStrLn (unlines bigBoard)
                    putStrLn "\nPressione Enter para voltar ao tabuleiro menor..."
                    _ <- getLine
                    timePassed <- checkTimeUp timeUpVar
                    if timePassed || secondsRemaining <= 0
                        then do
                            putStrLn "\n\x23F0 Tempo esgotado! Passando a vez..."
                            return Nothing
                        else loop b
                else do
                    timePassed <- checkTimeUp timeUpVar
                    if timePassed || secondsRemaining <= 0
                        then do
                            putStrLn "\n\x23F0 Tempo esgotado! Passando a vez..."
                            return Nothing
                        else do
                            putStr "Digite a linha (A-I): "
                            hFlush stdout
                            lineInput <- getLine

                            putStr "Digite a coluna (1-9): "
                            hFlush stdout
                            colInput <- getLine

                            let maybeCol = readMaybe colInput :: Maybe Int
                            case (lineInput, maybeCol) of
                                (l:_, Just c) ->
                                    case tryMoveSmall b (toUpper l) c player of
                                        Just (newBoard, venceu) -> do
                                            clearScreen
                                            putStrLn (unlines newBoard)
                                            if venceu
                                                then putStrLn $ "\x1F3C6 Vitória do jogador " ++ [player] ++ " no tabuleiro menor!"
                                                else putStrLn "Jogada no tabuleiro menor realizada!"
                                            threadDelay 1000000 -- pequena pausa antes de voltar
                                            return (Just newBoard)
                                        Nothing -> do
                                            putStrLn "--- JOGADA INVÁLIDA! Tente novamente. ---"
                                            threadDelay 1500000
                                            loop b
                                _ -> do
                                    putStrLn "--- ENTRADA INVÁLIDA! Não aperte Enter ---"
                                    threadDelay 1500000
                                    loop b

    loop board

-- Função auxiliar para verificar se o tempo acabou
checkTimeUp :: MVar () -> IO Bool
checkTimeUp var = not <$> isEmptyMVar var
