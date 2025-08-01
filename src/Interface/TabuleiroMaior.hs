module Interface.TabuleiroMaior where

import Types
import Arte (clearScreen)
import Data.Char (toUpper)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Interface.TabuleiroMenor (gameLoopSmall,
                                smallBoard1Template, smallBoard2Template, smallBoard3Template,
                                smallBoard4Template, smallBoard5Template, smallBoard6Template,
                                smallBoard7Template, smallBoard8Template, smallBoard9Template)

-- Mapeamento de índice (1-9) para o índice da lista (0-8)
getQuadrantIndex :: Int -> Maybe Int
getQuadrantIndex i = if i >= 1 && i <= 9 then Just (i - 1) else Nothing

-- Função para obter o template do tabuleiro menor com base no índice
getSmallBoardTemplate :: Int -> [String]
getSmallBoardTemplate index =
    if index == 1 then smallBoard1Template
    else if index == 2 then smallBoard2Template
    else if index == 3 then smallBoard3Template
    else if index == 4 then smallBoard4Template
    else if index == 5 then smallBoard5Template
    else if index == 6 then smallBoard6Template
    else if index == 7 then smallBoard7Template
    else if index == 8 then smallBoard8Template
    else smallBoard9Template

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
                        putStrLn $ "Acessando o quadrante " ++ show index ++ "..."
                        putStrLn "Pressione Enter para continuar..."
                        _ <- getLine
                        
                        let currentSmallBoard = smallBoards !! boardIndex
                        
                        newSmallBoard <- gameLoopSmall currentSmallBoard player

                        let newSmallBoards = replaceAtIndex boardIndex newSmallBoard smallBoards
                        
                        gameLoop bigBoard newSmallBoards (switchPlayer player)

                    Nothing -> do
                        putStrLn "--- ENTRADA INVÁLIDA! Use um número de 1 a 9. ---"
                        gameLoop bigBoard smallBoards player

            _ -> do
                putStrLn "--- ENTRADA INVÁLIDA! Use um número de 1 a 9. ---"
                gameLoop bigBoard smallBoards player