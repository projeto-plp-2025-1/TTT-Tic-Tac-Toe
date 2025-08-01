module Interface.Tabuleiro where

import Types
import Arte (clearScreen)
import Data.Char (toUpper)
import System.IO (hFlush, stdout) 
import Text.Read (readMaybe) 

-- Mapeamento de linha e coluna com os index's do template
getRowIndex :: Char -> Maybe Int
getRowIndex r = case toUpper r of
    'A' -> Just 2; 'B' -> Just 4; 'C' -> Just 6
    'D' -> Just 9; 'E' -> Just 11; 'F' -> Just 13
    'G' -> Just 16; 'H' -> Just 18; 'I' -> Just 20
    _   -> Nothing

getColIndex :: Int -> Maybe Int
getColIndex c = case c of
    1 -> Just 5; 2 -> Just 10; 3 -> Just 14
    4 -> Just 20; 5 -> Just 25; 6 -> Just 30
    7 -> Just 37; 8 -> Just 41; 9 -> Just 47
    _ -> Nothing

-- Substitui o caractere na posição i da string (tabuleiro) por um char (símbolo)
replaceAtIndex :: Int -> Char -> String -> String
replaceAtIndex i char str = take i str ++ [char] ++ drop (i + 1) str

-- Tenta fazer uma jogada
-- Retorna 'Just novoTabuleiro' em caso de sucesso
-- Retorna 'Nothing' se a jogada for inválida (coordenada ou espaço inválido)
tryMove :: [String] -> Char -> Int -> Char -> Maybe [String]
tryMove board line col player =
    case (getRowIndex line, getColIndex col) of
        (Just lineIndex, Just colIndex) ->
            let
                targetLine = board !! lineIndex
                -- Verifica se o espaço está vazio
                isCellEmpty = (targetLine !! colIndex) == ' '
            in 
                if isCellEmpty then
                    let
                        updatedLine = replaceAtIndex colIndex player targetLine
                        newBoard = take lineIndex board ++ [updatedLine] ++ drop (lineIndex + 1) board
                    in
                        Just newBoard
                else
                    -- Caso a célula já esteja ocupada
                    Nothing
        _ -> Nothing -- Coordenada inválida

-- | Alterna o jogador atual.
switchPlayer :: Char -> Char
switchPlayer 'X' = 'O'
switchPlayer 'O' = 'X'
switchPlayer other = other -- Caso base, não deve acontecer

-- O loop principal do jogo. É uma função recursiva que mantém o estado do jogo.
gameLoop :: [String] -> Char -> IO ()
gameLoop board player = do
    -- 1. Mostra o estado atual
    putStrLn "" -- Linha em branco para espaçamento
    putStrLn (unlines board)
    putStrLn $ "Turno do Jogador: " ++ [player]

    -- 2. Pede a entrada do usuário
    putStr "Digite a linha (A-I), ou 'Q' para retornar ao menu: "
    hFlush stdout
    lineInput <- getLine

    -- 3. Processa a entrada
    if not (null lineInput) && toUpper (head lineInput) == 'Q' then
        putStrLn "Obrigado por jogar!" -- Fim do jogo
    else do
        putStr "Digite a coluna (1-9): "
        hFlush stdout
        colInput <- getLine

        -- Tenta converter a entrada da coluna para um número
        let maybeCol = readMaybe colInput :: Maybe Int

        -- Valida a entrada completa
        case (lineInput, maybeCol) of
            -- A linha não pode ser vazia e a coluna tem que ser um número válido
            (l:_, Just c) -> do
                -- Tenta aplicar a jogada
                case tryMove board (toUpper l) c player of
                    Just newBoard ->
                        -- Sucesso! Continua o loop com o tabuleiro atualizado e o próximo jogador.
                        gameLoop newBoard (switchPlayer player)
                    Nothing -> do
                        -- Falha! Informa o erro e tenta de novo com o mesmo jogador.
                        putStrLn "--- JOGADA INVÁLIDA! (Posição não existe ou já está ocupada). Tente novamente. ---"
                        gameLoop board player
            _ -> do
                -- Falha na entrada! Informa o erro e tenta de novo com o mesmo jogador.
                putStrLn "--- ENTRADA INVÁLIDA! Use uma letra de A-I e um número de 1-9. ---"
                gameLoop board player
