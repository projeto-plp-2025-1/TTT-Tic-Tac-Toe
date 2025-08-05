{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
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
import qualified Utils.VerificacaoVitoria as VV

-- Converte uma entrada (1 a 9) para o índice correspondente (0 a 8)
getQuadrantIndex :: Int -> Maybe Int
getQuadrantIndex i = if i >= 1 && i <= 9 then Just (i - 1) else Nothing

-- Retorna o template do tabuleiro menor baseado no índice
getSmallBoardTemplate :: Int -> [String]
getSmallBoardTemplate index =
    case index of
        0 -> smallBoard1Template
        1 -> smallBoard2Template
        2 -> smallBoard3Template
        3 -> smallBoard4Template
        4 -> smallBoard5Template
        5 -> smallBoard6Template
        6 -> smallBoard7Template
        7 -> smallBoard8Template
        8 -> smallBoard9Template
        _ -> smallBoard9Template

-- Substitui um elemento em uma lista pelo novo valor na posição i
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i + 1) xs

-- Alterna entre os símbolos dos dois jogadores
switchPlayer :: Char -> Char -> Char -> Char
switchPlayer player1 player2 currentPlayer
    | currentPlayer == player1 = player2
    | otherwise                = player1

-- Localiza a célula que mudou entre o tabuleiro antigo e o novo (linha, coluna, símbolo)
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

-- Converte coordenadas de linha e coluna no tabuleiro menor para índice (0 a 8)
cellToIndex :: Int -> Int -> Maybe Int
cellToIndex line col = do
    lIdx <- case line of
        2 -> Just 0
        4 -> Just 1
        6 -> Just 2
        _ -> Nothing
    cIdx <- case col of
        5  -> Just 0
        10 -> Just 1
        14 -> Just 2
        _ -> Nothing
    Just (lIdx * 3 + cIdx)

-- Descobre o índice da célula modificada com base no tabuleiro antes e depois
getChangedCellIndex :: [String] -> [String] -> Maybe Int
getChangedCellIndex oldBoard newBoard = do
    (line, col, _) <- findChangedCell oldBoard newBoard
    cellToIndex line col

-- Atualiza o tabuleiro maior com o símbolo jogado no tabuleiro menor correspondente
updateBoard :: [String] -> Int -> [String] -> [String] -> Maybe [String]
updateBoard bigBoard quadrantIndex oldSmallBoard newSmallBoard = do
    -- Descobre a célula que mudou no tabuleiro menor
    (changedLineTemplate, changedColTemplate, playerChar) <- findChangedCell oldSmallBoard newSmallBoard

    -- Funções para mapear linha e coluna do tabuleiro menor para o tabuleiro maior, com base no quadrante
    let lineMapper = case quadrantIndex of
            0 -> \x -> case x of { 2 -> Just 2; 4 -> Just 4; 6 -> Just 6; _ -> Nothing }
            1 -> \x -> case x of { 2 -> Just 2; 4 -> Just 4; 6 -> Just 6; _ -> Nothing }
            2 -> \x -> case x of { 2 -> Just 2; 4 -> Just 4; 6 -> Just 6; _ -> Nothing }
            3 -> \x -> case x of { 2 -> Just 9; 4 -> Just 11; 6 -> Just 13; _ -> Nothing }
            4 -> \x -> case x of { 2 -> Just 9; 4 -> Just 11; 6 -> Just 13; _ -> Nothing }
            5 -> \x -> case x of { 2 -> Just 9; 4 -> Just 11; 6 -> Just 13; _ -> Nothing }
            6 -> \x -> case x of { 2 -> Just 16; 4 -> Just 18; 6 -> Just 20; _ -> Nothing }
            7 -> \x -> case x of { 2 -> Just 16; 4 -> Just 18; 6 -> Just 20; _ -> Nothing }
            8 -> \x -> case x of { 2 -> Just 16; 4 -> Just 18; 6 -> Just 20; _ -> Nothing }
            _ -> const Nothing

    let colMapper = case quadrantIndex of
            0 -> \x -> case x of { 5 -> Just 5; 10 -> Just 10; 14 -> Just 14; _ -> Nothing }
            1 -> \x -> case x of { 5 -> Just 20; 10 -> Just 25; 14 -> Just 30; _ -> Nothing }
            2 -> \x -> case x of { 5 -> Just 35; 10 -> Just 40; 14 -> Just 44; _ -> Nothing }
            3 -> \x -> case x of { 5 -> Just 5; 10 -> Just 10; 14 -> Just 14; _ -> Nothing }
            4 -> \x -> case x of { 5 -> Just 20; 10 -> Just 25; 14 -> Just 30; _ -> Nothing }
            5 -> \x -> case x of { 5 -> Just 35; 10 -> Just 40; 14 -> Just 44; _ -> Nothing }
            6 -> \x -> case x of { 5 -> Just 5; 10 -> Just 10; 14 -> Just 14; _ -> Nothing }
            7 -> \x -> case x of { 5 -> Just 20; 10 -> Just 25; 14 -> Just 30; _ -> Nothing }
            8 -> \x -> case x of { 5 -> Just 35; 10 -> Just 40; 14 -> Just 44; _ -> Nothing }
            _ -> const Nothing

    -- Mapeia linha e coluna alteradas
    l <- lineMapper changedLineTemplate
    c <- colMapper changedColTemplate

    let targetLine = bigBoard !! l

    -- Verifica se posição está vazia
    if targetLine !! c /= ' '
        then Nothing
        else
            let updatedLine = replaceAtIndex c playerChar targetLine
                updatedBigBoard = replaceAtIndex l updatedLine bigBoard
            in Just updatedBigBoard

-- Inicializa lista de vencedores dos tabuleiros menores (Nothing = sem vencedor)
type WinnerBoard = [Maybe Char]

inicializaWinnerBoard :: WinnerBoard
inicializaWinnerBoard = replicate 9 Nothing

-- Atualiza vencedor de um quadrante, se houve vitória naquele quadrante
atualizaWinnerBoard :: WinnerBoard -> Int -> [String] -> Char -> WinnerBoard
atualizaWinnerBoard winners idx smallBoard jogador =
    if VV.verificarVitoria smallBoard jogador
       then replaceAtIndex idx (Just jogador) winners
       else winners

-- Verifica se jogador venceu o tabuleiro maior com base na lista de vencedores
verificarVitoriaMaior :: WinnerBoard -> Char -> Bool
verificarVitoriaMaior winners jogador =
    let
        combinacoes =
            [ [0,1,2]
            , [3,4,5]
            , [6,7,8]
            , [0,3,6]
            , [1,4,7]
            , [2,5,8]
            , [0,4,8]
            , [2,4,6]
            ]
        ganhouLinha line = all (\i -> winners !! i == Just jogador) line
    in
        any ganhouLinha combinacoes

-- Adaptação do gameLoop para incluir WinnerBoard
gameLoop :: [String]       -- tabuleiro maior
         -> [[String]]     -- lista dos 9 tabuleiros menores
         -> Char           -- símbolo do jogador 1
         -> Char           -- símbolo do jogador 2
         -> Char           -- jogador atual (símbolo)
         -> Maybe Int      -- quadrante permitido para jogar
         -> String         -- nome do jogador 1
         -> String         -- nome do jogador 2
         -> WinnerBoard    -- lista de vencedores dos tabuleiros menores
         -> IO ()
gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer maybeNextQuadrant name1 name2 winnerBoard = do
    clearScreen
    putStrLn ""
    putStrLn (unlines bigBoard)

    let currentPlayerName = if currentPlayer == player1Symbol then name1 else name2
    putStrLn $ "Turno de: " ++ currentPlayerName ++ " [" ++ [currentPlayer] ++ "]"

    case maybeNextQuadrant of
        Just idx -> putStrLn $ "Você deve jogar no quadrante: " ++ show (idx + 1)
        Nothing  -> putStrLn "Você pode jogar em qualquer quadrante."

    putStr "Escolha o quadrante (1-9) que quer jogar ou 'Q' para sair: "
    hFlush stdout
    input <- getLine

    if not (null input) && toUpper (head input) == 'Q' then
        putStrLn "Obrigado por jogar!"
    else do
        let maybeIndex = reads input :: [(Int, String)]
        case maybeIndex of
            [(index, "")] -> do
                let boardIndex = index - 1
                if boardIndex < 0 || boardIndex >= length smallBoards then do
                    putStrLn "\n--- ENTRADA INVÁLIDA! Use um número de 1 a 9. ---"
                    _ <- getLine
                    gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer maybeNextQuadrant name1 name2 winnerBoard
                else if maybeNextQuadrant /= Nothing && maybeNextQuadrant /= Just boardIndex then do
                    putStrLn "\nVocê deve jogar no quadrante determinado pelo movimento anterior."
                    putStrLn "Pressione Enter para continuar..."
                    _ <- getLine
                    gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer maybeNextQuadrant name1 name2 winnerBoard
                else do
                    putStrLn $ "\n--- Acessando o quadrante " ++ show index ++ " ---"
                    putStrLn "Pressione Enter para continuar..."
                    _ <- getLine

                    let currentSmallBoard = smallBoards !! boardIndex

                    maybeNewSmallBoard <- gameLoopSmall currentSmallBoard currentPlayer bigBoard

                    case maybeNewSmallBoard of
                        Just newBoard -> do
                            let newSmallBoards = replaceAtIndex boardIndex newBoard smallBoards

                            -- Atualiza o WinnerBoard com possível vitória no tabuleiro menor
                            let newWinnerBoard = atualizaWinnerBoard winnerBoard boardIndex newBoard currentPlayer

                            -- Verifica vitória no tabuleiro maior
                            if verificarVitoriaMaior newWinnerBoard currentPlayer then do
                                clearScreen
                                putStrLn (unlines bigBoard)
                                putStrLn $ "\n🏆 Parabéns, jogador " ++ [currentPlayer] ++ " venceu o jogo! 🏆"
                            else
                                case updateBoard bigBoard boardIndex currentSmallBoard newBoard of
                                    Just newBigBoard -> do
                                        let nextPlayer = switchPlayer player1Symbol player2Symbol currentPlayer
                                        let nextQuadrant = getChangedCellIndex currentSmallBoard newBoard
                                        gameLoop newBigBoard newSmallBoards player1Symbol player2Symbol nextPlayer nextQuadrant name1 name2 newWinnerBoard
                                    Nothing -> do
                                        putStrLn "Erro: posição já ocupada no tabuleiro maior!"
                                        putStrLn "Pressione Enter para continuar..."
                                        _ <- getLine
                                        gameLoop bigBoard newSmallBoards player1Symbol player2Symbol currentPlayer maybeNextQuadrant name1 name2 winnerBoard
                        Nothing -> do
                            putStrLn "\nTempo esgotado. Passando a vez..."
                            putStrLn "Pressione Enter para continuar..."
                            _ <- getLine
                            let nextPlayer = switchPlayer player1Symbol player2Symbol currentPlayer
                            gameLoop bigBoard smallBoards player1Symbol player2Symbol nextPlayer maybeNextQuadrant name1 name2 winnerBoard

            _ -> do
                putStrLn "\n--- ENTRADA INVÁLIDA! Use um número de 1 a 9. ---"
                _ <- getLine
                gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer maybeNextQuadrant name1 name2 winnerBoard
