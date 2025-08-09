{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
module Core.TabuleiroMaior where

import Interface.Arte (clearScreen, exibirVencedor, exibirVelha)
import Data.Char (toLower)  
import Core.Bot 
import System.IO (hFlush, stdout)
import Core.TabuleiroMenor (gameLoopSmall,
                            smallBoard1Template, smallBoard2Template, smallBoard3Template,
                            smallBoard4Template, smallBoard5Template, smallBoard6Template,
                            smallBoard7Template, smallBoard8Template, smallBoard9Template)
import qualified Utils.VerificacaoVitoria as VV
import qualified Core.Persistencia as P
import qualified Core.Salvamento as S
import qualified Utils.Types as Type
import Utils.Types (SmallBoardState, QuadrantState (..))
import Data.Int (Int)

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

-- Atualiza o estado do jogo quando um jogador vence um tabuleiro menor
atualizaWinner :: SmallBoardState -> Int -> [String] -> Char -> Char -> Int -> Int 
              -> (SmallBoardState, Int, Int)
atualizaWinner winners idx smallBoard currentPlayer player1Symbol j1SW j2SW =
    if VV.verificarVitoria smallBoard currentPlayer
       then (replaceAtIndex idx (Winner currentPlayer) winners,
            if currentPlayer == player1Symbol then j1SW + 1 else j1SW,
            if currentPlayer == player1Symbol then j2SW else j2SW + 1)
       else (winners, j1SW, j2SW)

-- Função auxiliar para atualizar apenas o winnerBoard
atualizaWinnerBoard :: SmallBoardState -> Int -> [String] -> Char -> SmallBoardState
atualizaWinnerBoard winners idx smallBoard jogador =
    if VV.verificarVitoria smallBoard jogador
       then replaceAtIndex idx (Winner jogador) winners
       else winners

atualizaDrawBoard :: SmallBoardState -> Int -> [String] -> SmallBoardState
atualizaDrawBoard winners idx smallBoard =
    if verificarDraw smallBoard && winners !! idx == InProgress
       then replaceAtIndex idx Draw winners
       else winners

vencedorPorContagem :: Int -> Int -> Char -> Char -> Maybe Char
vencedorPorContagem p1 p2 player1Symbol player2Symbol
    | p1 > p2  = Just player1Symbol
    | p2 > p1  = Just player2Symbol
    | otherwise = Nothing

exibirPlacar :: Char -> String -> Int -> Char -> String -> Int -> IO ()
exibirPlacar player1Symbol nome1 p1SmallWins player2Symbol nome2 p2SmallWins = do
    putStrLn "\n------- PLACAR -------"
    if p1SmallWins >= p2SmallWins
        then do
            putStrLn $ nome1 ++ " " ++ show player1Symbol ++ ": " ++ show p1SmallWins ++ " vitórias"
            putStrLn $ nome2 ++ " " ++ show player2Symbol ++  ": " ++ show p2SmallWins ++ " vitórias"
        else do
            putStrLn $ nome2 ++ " " ++ show player2Symbol ++  ": " ++ show p2SmallWins ++ " vitórias"
            putStrLn $ nome1 ++ " " ++ show player1Symbol ++  ": " ++ show p1SmallWins ++ " vitórias"
    putStrLn ""

verificarDraw :: [String] -> Bool
verificarDraw board = 
    all (posicaoDraw board) [(2,5), (2,10), (2,14), (4,5), (4,10), (4,14), (6,5), (6,10), (6,14)]

posicaoDraw :: [String] -> (Int, Int) -> Bool
posicaoDraw board (linha, coluna) =
    (board !! linha) !! coluna /= ' '


verificarVitoriaMaior :: [QuadrantState] -> Char -> Bool
verificarVitoriaMaior winners jogador =
    let
        combinacoes =
            [ [0,1,2]  -- Linha superior
            , [3,4,5]  -- Linha do meio
            , [6,7,8]  -- Linha inferior
            , [0,3,6]  -- Coluna esquerda
            , [1,4,7]  -- Coluna do meio
            , [2,5,8]  -- Coluna direita
            , [0,4,8]  -- Diagonal principal
            , [2,4,6]  -- Diagonal secundária
            ]
        ganhouLinha line = all (\i -> 
            case 
                winners !! i of
                Winner c -> c == jogador
                _ -> False) line
    in
        any ganhouLinha combinacoes

todosQuadrantesFinalizados :: [QuadrantState] -> Bool
todosQuadrantesFinalizados = all $ \case
    InProgress -> False
    Draw -> True
    Winner _ -> True

gameLoop :: [String]       -- tabuleiro maior
         -> [[String]]     -- lista dos 9 tabuleiros menores
         -> Char           -- símbolo do jogador 1
         -> Char           -- símbolo do jogador 2
         -> Char           -- jogador atual (símbolo)
         -> Maybe Int      -- quadrante permitido para jogar
         -> String         -- nome do jogador 1
         -> String         -- nome do jogador 2
         -> Int            -- Sub Tabuleiros Dominados do j1
         -> Int            -- Sub Tabuleiros Dominados do j2
         -> SmallBoardState    -- lista de vencedores dos tabuleiros menores
         -> IO ()
gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer maybeNextQuadrant name1 name2 j1SmallWin j2SmallWin winnerBoard = do
    clearScreen
    putStrLn ""
    putStrLn (unlines bigBoard)

    
    let currentPlayerName = if currentPlayer == player1Symbol then name1 else name2

    -- Ajusta quadrante obrigatório para liberar se já foi vencido
    let adjustedNextQuadrant = case maybeNextQuadrant of
            Just idx -> case winnerBoard !! idx of
                     InProgress -> Just idx  
                     _ -> Nothing          
            Nothing  -> Nothing

    if currentPlayerName == "Bot" then do
        (chosenQuadrant, newSmallBoard) <- botTakeTurn bigBoard smallBoards currentPlayer adjustedNextQuadrant winnerBoard
        let currentSmallBoard = smallBoards !! chosenQuadrant
            newSmallBoards = replaceAtIndex chosenQuadrant newSmallBoard smallBoards
            newWinnerBoard = atualizaWinnerBoard winnerBoard chosenQuadrant newSmallBoard currentPlayer

        -- Verifica vitória do bot no tabuleiro maior
        if verificarVitoriaMaior newWinnerBoard currentPlayer then do
            clearScreen
            putStrLn (unlines bigBoard)
            exibirVencedor currentPlayerName
            P.registrarVitoria currentPlayerName

        -- Verifica empate (todos quadrantes finalizados)
        else if todosQuadrantesFinalizados newWinnerBoard then do
            clearScreen
            putStrLn (unlines bigBoard)
            exibirVelha

        else
            case updateBoard bigBoard chosenQuadrant currentSmallBoard newSmallBoard of
            Just newBigBoard -> do
                let nextPlayer = switchPlayer player1Symbol player2Symbol currentPlayer
                    nextQuadrant = getChangedCellIndex currentSmallBoard newSmallBoard
                gameLoop newBigBoard newSmallBoards player1Symbol player2Symbol nextPlayer nextQuadrant name1 name2 j1SmallWin j2SmallWin newWinnerBoard
            Nothing -> do
                putStrLn "Bot fez uma jogada inválida!"
                let nextPlayer = switchPlayer player1Symbol player2Symbol currentPlayer
                gameLoop bigBoard smallBoards player1Symbol player2Symbol nextPlayer adjustedNextQuadrant name1 name2 j1SmallWin j2SmallWin winnerBoard

        else do
            putStrLn $ "Turno de: " ++ currentPlayerName ++ " [" ++ [currentPlayer] ++ "]"

            case adjustedNextQuadrant of
                Just idx -> putStrLn $ "Você deve jogar no quadrante: " ++ show (idx + 1)
                Nothing  -> putStrLn "Você pode jogar em qualquer quadrante."

            putStrLn "Digite o número do quadrante (1-9), 'salvar' para salvar, 'Q' para sair ou 'placar' (Utilizado em Desempate):"
            putStr "→ "
            hFlush stdout
            input <- getLine

            case map toLower input of
                "q" -> putStrLn "Obrigado por jogar!"
                "salvar" -> do
                    let gameState = Type.GameState
                            (player1Symbol, name1)
                            j1SmallWin
                            (player2Symbol, name2)
                            j2SmallWin
                            currentPlayer
                            adjustedNextQuadrant
                            bigBoard
                            smallBoards
                            winnerBoard
                    S.salvarJogo gameState
                    putStrLn "Jogo salvo com sucesso! Pressione ENTER para continuar."
                    _ <- getLine
                    gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer adjustedNextQuadrant name1 name2 j1SmallWin j2SmallWin winnerBoard
                "placar" -> do
                    exibirPlacar player1Symbol name1 j1SmallWin player2Symbol name2  j2SmallWin
                    putStrLn "Pressione ENTER para continuar."
                    _ <- getLine
                    gameLoop bigBoard smallBoards player1Symbol player2Symbol 
                            currentPlayer adjustedNextQuadrant name1 name2 
                            j1SmallWin j2SmallWin winnerBoard

                _ -> do
                    let maybeIndex = reads input :: [(Int, String)]
                    case maybeIndex of
                        [(index, "")] -> do
                            let boardIndex = index - 1
                            if boardIndex < 0 || boardIndex >= length smallBoards then do
                                putStrLn "\n--- ENTRADA INVÁLIDA! Use um número de 1 a 9. ---"
                                _ <- getLine
                                gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer adjustedNextQuadrant name1 name2 j1SmallWin j2SmallWin winnerBoard

                            else if adjustedNextQuadrant /= Nothing && adjustedNextQuadrant /= Just boardIndex then do
                                putStrLn "\nVocê deve jogar no quadrante determinado pelo movimento anterior."
                                putStrLn "Pressione Enter para continuar..."
                                _ <- getLine
                                gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer adjustedNextQuadrant name1 name2 j1SmallWin j2SmallWin winnerBoard

                            else if winnerBoard !! boardIndex == Draw then do
                                putStrLn "\nEsse quadrante já está cheio! Escolha outro."
                                putStrLn "Pressione Enter para continuar..."
                                _ <- getLine
                                gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer adjustedNextQuadrant name1 name2 j1SmallWin j2SmallWin winnerBoard

                            else if case winnerBoard !! boardIndex of
                                InProgress -> False
                                _ -> True then do
                                putStrLn "\nEsse quadrante já foi vencido! Escolha outro."
                                putStrLn "Pressione Enter para continuar..."
                                _ <- getLine
                                gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer adjustedNextQuadrant name1 name2 j1SmallWin j2SmallWin winnerBoard
                            
                            else do
                                putStrLn $ "\n--- Acessando o quadrante " ++ show index ++ " ---"
                                putStrLn "Pressione Enter para continuar..."
                                _ <- getLine

                                let currentSmallBoard = smallBoards !! boardIndex

                                maybeNewSmallBoard <- gameLoopSmall currentSmallBoard currentPlayer bigBoard

                                case maybeNewSmallBoard of
                                    Just newBoard -> do
                                        let newSmallBoards = replaceAtIndex boardIndex newBoard smallBoards
                                        let drawWinnerBoard = atualizaDrawBoard winnerBoard boardIndex newBoard
                                        let (newWinnerBoard, newj1SmallWin, newj2SmallWin) = atualizaWinner drawWinnerBoard boardIndex newBoard currentPlayer player1Symbol j1SmallWin j2SmallWin
                                        
                                        if verificarVitoriaMaior newWinnerBoard currentPlayer then do
                                            clearScreen
                                            putStrLn (unlines bigBoard)
                                            exibirVencedor currentPlayerName
                                            P.registrarVitoria currentPlayerName
                                        else if todosQuadrantesFinalizados newWinnerBoard then do
                                            clearScreen
                                            putStrLn (unlines bigBoard)
                                                
                                            case vencedorPorContagem j1SmallWin j2SmallWin player1Symbol player2Symbol of
                                                Just winnerSymbol -> do
                                                    let winnerName = if winnerSymbol == player1Symbol then name1 else name2
                                                    exibirVencedor winnerName
                                                    P.registrarVitoria winnerName
                                                Nothing -> 
                                                    exibirVelha
                                        else
                                            case updateBoard bigBoard boardIndex currentSmallBoard newBoard of
                                                Just newBigBoard -> do
                                                    let nextPlayer = switchPlayer player1Symbol player2Symbol currentPlayer
                                                    let nextQuadrant = getChangedCellIndex currentSmallBoard newBoard
                                                    gameLoop newBigBoard newSmallBoards player1Symbol player2Symbol nextPlayer nextQuadrant name1 name2 newj1SmallWin newj2SmallWin newWinnerBoard
                                                Nothing -> do
                                                    putStrLn "Erro: posição já ocupada no tabuleiro maior!"
                                                    putStrLn "Pressione Enter para continuar..."
                                                    _ <- getLine
                                                    gameLoop bigBoard newSmallBoards player1Symbol player2Symbol currentPlayer adjustedNextQuadrant name1 name2 j1SmallWin j2SmallWin winnerBoard
                                    Nothing -> do
                                        putStrLn "\nTempo esgotado. Passando a vez..."
                                        putStrLn "Pressione Enter para continuar..."
                                        _ <- getLine
                                        let nextPlayer = switchPlayer player1Symbol player2Symbol currentPlayer
                                        gameLoop bigBoard smallBoards player1Symbol player2Symbol nextPlayer adjustedNextQuadrant name1 name2 j1SmallWin j2SmallWin winnerBoard
                        _ -> do
                            putStrLn "\n--- ENTRADA INVÁLIDA! Use um número de 1 a 9. ---"
                            _ <- getLine
                            gameLoop bigBoard smallBoards player1Symbol player2Symbol currentPlayer adjustedNextQuadrant name1 name2 j1SmallWin j2SmallWin winnerBoard