module Core.TabuleiroMaior where

import Utils.Types
import Interface.Arte (clearScreen)
import Data.Char (toUpper)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Core.TabuleiroMenor (gameLoopSmall,
                            smallBoard1Template, smallBoard2Template, smallBoard3Template,
                            smallBoard4Template, smallBoard5Template, smallBoard6Template,
                            smallBoard7Template, smallBoard8Template, smallBoard9Template)

getQuadrantIndex :: Int -> Maybe Int
getQuadrantIndex i = if i >= 1 && i <= 9 then Just (i - 1) else Nothing

getSmallBoardTemplate :: Int -> [String]
getSmallBoardTemplate index =
    case index of
        1 -> smallBoard1Template
        2 -> smallBoard2Template
        3 -> smallBoard3Template
        4 -> smallBoard4Template
        5 -> smallBoard5Template
        6 -> smallBoard6Template
        7 -> smallBoard7Template
        8 -> smallBoard8Template
        _ -> smallBoard9Template

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i + 1) xs

switchPlayer :: Char -> Char
switchPlayer 'X' = 'O'
switchPlayer 'O' = 'X'
switchPlayer other = other

gameLoop :: [String] -> [[String]] -> Char -> IO ()
gameLoop bigBoard smallBoards player = do
    clearScreen
    putStrLn ""
    putStrLn (unlines bigBoard)
    putStrLn $ "Turno do Jogador: " ++ [player]

    putStr "Escolha o quadrante (1-9) que quer jogar ou 'Q' para sair: "
    hFlush stdout
    input <- getLine

    if not (null input) && toUpper (head input) == 'Q' then
        putStrLn "Obrigado por jogar!"
    else do
        let maybeIndex = readMaybe input :: Maybe Int

        case maybeIndex of
            Just index ->
                case getQuadrantIndex index of
                    Just boardIndex -> do
                        putStrLn $ "\n--- Acessando o quadrante " ++ show index ++ " ---"
                        putStrLn "Pressione Enter para continuar..."
                        _ <- getLine

                        let currentSmallBoard = smallBoards !! boardIndex

                        maybeNewSmallBoard <- gameLoopSmall currentSmallBoard player

                        case maybeNewSmallBoard of
                            Just newBoard -> do
                                let newSmallBoards = replaceAtIndex boardIndex newBoard smallBoards
                                gameLoop bigBoard newSmallBoards (switchPlayer player)
                            Nothing -> do
                                putStrLn "\nVocê voltou para o tabuleiro maior sem jogar."
                                putStrLn "Pressione Enter para continuar..."
                                _ <- getLine
                                gameLoop bigBoard smallBoards player

                    Nothing -> do
                        putStrLn "\n--- ENTRADA INVÁLIDA! Use um número de 1 a 9. ---"
                        gameLoop bigBoard smallBoards player

            Nothing -> do
                putStrLn "\n--- ENTRADA INVÁLIDA! Use um número de 1 a 9. ---"
                gameLoop bigBoard smallBoards player

