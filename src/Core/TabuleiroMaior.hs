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

-- Função que encontra qual célula mudou entre dois tabuleiros menores (templates)
findChangedCell :: [String] -> [String] -> Maybe (Int, Int, Char)
findChangedCell oldBoard newBoard =
    let indexedRows = zip [0..] (zip oldBoard newBoard)
        findInRow (rowIndex, (oldLine, newLine)) =
            let indexedCols = zip3 [0..] oldLine newLine
                changed = filter (\(_, o, n) -> o /= n) indexedCols
            in case changed of
                ((colIndex, _, newChar):_) -> Just (rowIndex, colIndex, newChar)
                _ -> Nothing
    in foldr (\row acc -> case acc of Just _ -> acc; Nothing -> findInRow row) Nothing indexedRows

-- Mapeamento de linhas e colunas do quadrante 1 para o maior
lineTemplateToSmallIndexQuad1 :: Int -> Maybe Int
lineTemplateToSmallIndexQuad1 lineIndex = case lineIndex of
    2 -> Just 2
    4 -> Just 4
    6 -> Just 6
    _ -> Nothing

colTemplateToSmallIndexQuad1 :: Int -> Maybe Int
colTemplateToSmallIndexQuad1 colIndex = case colIndex of
    5  -> Just 5
    10 -> Just 10
    14 -> Just 14
    _  -> Nothing

-- Mapeamento de linhas e colunas do quadrante 2 para o maior
lineTemplateToSmallIndexQuad2 :: Int -> Maybe Int
lineTemplateToSmallIndexQuad2 lineIndex = case lineIndex of
    2 -> Just 2
    4 -> Just 4
    6 -> Just 6
    _ -> Nothing

colTemplateToSmallIndexQuad2 :: Int -> Maybe Int
colTemplateToSmallIndexQuad2 colIndex = case colIndex of
    5  -> Just 20
    10 -> Just 25
    14 -> Just 30
    _  -> Nothing

-- Mapeamento de linhas e colunas do quadrante 3 para o maior
lineTemplateToSmallIndexQuad3 :: Int -> Maybe Int
lineTemplateToSmallIndexQuad3 lineIndex = case lineIndex of
    2 -> Just 2
    4 -> Just 4
    6 -> Just 6
    _ -> Nothing

colTemplateToSmallIndexQuad3 :: Int -> Maybe Int
colTemplateToSmallIndexQuad3 colIndex = case colIndex of
    5  -> Just 36
    10 -> Just 40
    14 -> Just 44
    _  -> Nothing

-- Mapeamento de linhas e colunas do quadrante 4 para o maior
lineTemplateToSmallIndexQuad4 :: Int -> Maybe Int
lineTemplateToSmallIndexQuad4 lineIndex = case lineIndex of
    2 -> Just 8
    4 -> Just 10
    6 -> Just 12
    _ -> Nothing

colTemplateToSmallIndexQuad4 :: Int -> Maybe Int
colTemplateToSmallIndexQuad4 colIndex = case colIndex of
    5  -> Just 5
    10 -> Just 10
    14 -> Just 14
    _  -> Nothing

-- Mapeamento de linhas e colunas do quadrante 5 para o maior
lineTemplateToSmallIndexQuad5 :: Int -> Maybe Int
lineTemplateToSmallIndexQuad5 lineIndex = case lineIndex of
    2 -> Just 8
    4 -> Just 10
    6 -> Just 12
    _ -> Nothing

colTemplateToSmallIndexQuad5 :: Int -> Maybe Int
colTemplateToSmallIndexQuad5 colIndex = case colIndex of
    5  -> Just 20
    10 -> Just 25
    14 -> Just 30
    _  -> Nothing

-- Mapeamento de linhas e colunas do quadrante 6 para o maior
lineTemplateToSmallIndexQuad6 :: Int -> Maybe Int
lineTemplateToSmallIndexQuad6 lineIndex = case lineIndex of
    2 -> Just 8
    4 -> Just 10
    6 -> Just 12
    _ -> Nothing

colTemplateToSmallIndexQuad6 :: Int -> Maybe Int
colTemplateToSmallIndexQuad6 colIndex = case colIndex of
    5  -> Just 36
    10 -> Just 40
    14 -> Just 44
    _  -> Nothing

-- Mapeamento de linhas e colunas do quadrante 7 para o maior
lineTemplateToSmallIndexQuad7 :: Int -> Maybe Int
lineTemplateToSmallIndexQuad7 lineIndex = case lineIndex of
    2 -> Just 14
    4 -> Just 16
    6 -> Just 18
    _ -> Nothing

colTemplateToSmallIndexQuad7 :: Int -> Maybe Int
colTemplateToSmallIndexQuad7 colIndex = case colIndex of
    5  -> Just 5
    10 -> Just 10
    14 -> Just 14
    _  -> Nothing

-- Mapeamento de linhas e colunas do quadrante 8 para o maior
lineTemplateToSmallIndexQuad8 :: Int -> Maybe Int
lineTemplateToSmallIndexQuad8 lineIndex = case lineIndex of
    2 -> Just 14
    4 -> Just 16
    6 -> Just 18
    _ -> Nothing

colTemplateToSmallIndexQuad8 :: Int -> Maybe Int
colTemplateToSmallIndexQuad8 colIndex = case colIndex of
    5  -> Just 20
    10 -> Just 25
    14 -> Just 30
    _  -> Nothing

-- Mapeamento de linhas e colunas do quadrante 9 para o maior
lineTemplateToSmallIndexQuad9 :: Int -> Maybe Int
lineTemplateToSmallIndexQuad9 lineIndex = case lineIndex of
    2 -> Just 14
    4 -> Just 16
    6 -> Just 18
    _ -> Nothing

colTemplateToSmallIndexQuad9 :: Int -> Maybe Int
colTemplateToSmallIndexQuad9 colIndex = case colIndex of
    5  -> Just 36
    10 -> Just 40
    14 -> Just 44
    _  -> Nothing

-- Função para padronizar o mapeamento de linhas
getLineMapper :: Int -> (Int -> Maybe Int)
getLineMapper q = case q of
    0 -> lineTemplateToSmallIndexQuad1
    1 -> lineTemplateToSmallIndexQuad2
    2 -> lineTemplateToSmallIndexQuad3
    3 -> lineTemplateToSmallIndexQuad4
    4 -> lineTemplateToSmallIndexQuad5
    5 -> lineTemplateToSmallIndexQuad6
    6 -> lineTemplateToSmallIndexQuad7
    7 -> lineTemplateToSmallIndexQuad8
    8 -> lineTemplateToSmallIndexQuad9
    _ -> \_ -> Nothing

-- Função para padronizar o mapeamento de colunas
getColMapper :: Int -> (Int -> Maybe Int)
getColMapper q = case q of
    0 -> colTemplateToSmallIndexQuad1
    1 -> colTemplateToSmallIndexQuad2
    2 -> colTemplateToSmallIndexQuad3
    3 -> colTemplateToSmallIndexQuad4
    4 -> colTemplateToSmallIndexQuad5
    5 -> colTemplateToSmallIndexQuad6
    6 -> colTemplateToSmallIndexQuad7
    7 -> colTemplateToSmallIndexQuad8
    8 -> colTemplateToSmallIndexQuad9
    _ -> \_ -> Nothing


updateBoard :: [String] -> Int -> [String] -> [String] -> Maybe [String]
updateBoard bigBoard quadrantIndex oldSmallBoard newSmallBoard = do
    (changedLineTemplate, changedColTemplate, playerChar) <- findChangedCell oldSmallBoard newSmallBoard

    let lineMapper = getLineMapper quadrantIndex
    let colMapper  = getColMapper quadrantIndex

    -- Mapeia linha e coluna diretamente
    l <- lineMapper changedLineTemplate
    c <- colMapper changedColTemplate

    -- Atualiza a célula correta no tabuleiro maior
    let targetLine = bigBoard !! l

    if targetLine !! c /= ' '
        then Nothing
        else
            let updatedLine = replaceAtIndex c playerChar targetLine
                updatedBigBoard = replaceAtIndex l updatedLine bigBoard
            in Just updatedBigBoard

-- Atualizado para receber player1Symbol, player2Symbol e currentPlayerSymbol
gameLoop :: [String] -> [[String]] -> Char -> Char -> Char -> IO ()
gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer = do
    clearScreen
    putStrLn ""
    putStrLn (unlines bigBoard)
    putStrLn $ "Turno do Jogador: " ++ [currentPlayer]

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

                        maybeNewSmallBoard <- gameLoopSmall currentSmallBoard currentPlayer

                        case maybeNewSmallBoard of
                            Just newBoard -> do
                                let newSmallBoards = replaceAtIndex boardIndex newBoard smallBoards
                                case updateBoard bigBoard boardIndex currentSmallBoard newBoard of
                                    Just newBigBoard -> do
                                        let nextPlayer = if currentPlayer == player1Symbol then player2Symbol else player1Symbol
                                        gameLoop newBigBoard newSmallBoards player1Symbol player2Symbol nextPlayer
                                    Nothing -> do
                                        putStrLn "Erro: posição já ocupada no tabuleiro maior!"
                                        putStrLn "Pressione Enter para continuar..."
                                        _ <- getLine
                                        gameLoop bigBoard newSmallBoards player1Symbol player2Symbol currentPlayer
                            Nothing -> do
                                putStrLn "\nVocê voltou para o tabuleiro maior sem jogar."
                                putStrLn "Pressione Enter para continuar..."
                                _ <- getLine
                                gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer

                    Nothing -> do
                        putStrLn "\n--- ENTRADA INVÁLIDA! Use um número de 1 a 9. ---"
                        gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer

            Nothing -> do
                putStrLn "\n--- ENTRADA INVÁLIDA! Use um número de 1 a 9. ---"
                gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer
