module Core.Bot
  ( botTakeTurn
  ) where

import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Coordenadas dentro do tabuleiro pequeno para as células na sua grade 9x3 de strings:
coords :: [(Int, Int)]
coords = [(2,5),(2,10),(2,14),
          (4,5),(4,10),(4,14),
          (6,5),(6,10),(6,14)]

-- Linhas para verificar vitórias no tabuleiro pequeno (índices na lista coords):
winningLines :: [[Int]]
winningLines =
  [ [0,1,2]
  , [3,4,5]
  , [6,7,8]
  , [0,3,6]
  , [1,4,7]
  , [2,5,8]
  , [0,4,8]
  , [2,4,6]
  ]

-- Extrai o conteúdo da célula do tabuleiro pelo índice das coords
cellAt :: [String] -> Int -> Char
cellAt board idx =
  let (l,c) = coords !! idx
  in (board !! l) !! c

-- Substitui o caractere no índice na string
replaceAtIndexChar :: Int -> Char -> String -> String
replaceAtIndexChar i ch str = take i str ++ [ch] ++ drop (i + 1) str

-- Coloca o botSymbol nas coordenadas dadas no tabuleiro pequeno
placeSymbol :: [String] -> Int -> Char -> [String]
placeSymbol board idx symbol =
  let (l,c) = coords !! idx
      oldLine = board !! l
      newLine = replaceAtIndexChar c symbol oldLine
  in take l board ++ [newLine] ++ drop (l+1) board

-- Encontra uma jogada vencedora para um símbolo (bot ou oponente) em um tabuleiro pequeno
findWinningMove :: [String] -> Char -> Maybe Int
findWinningMove board symbol =
  let isLineWinning line = 
        let cells = map (cellAt board) line
            countSymbol = length (filter (== symbol) cells)
            countEmpty = length (filter (== ' ') cells)
        in countSymbol == 2 && countEmpty == 1
      findEmptyInLine line = 
        listToMaybe [idx | idx <- line, cellAt board idx == ' ']
  in
    listToMaybe [emptyIdx | line <- winningLines, isLineWinning line, let Just emptyIdx = findEmptyInLine line]

-- Escolhe uma célula para bloquear a jogada vencedora do oponente
findBlockingMove :: [String] -> Char -> Char -> Maybe Int
findBlockingMove board botSymbol opponentSymbol = findWinningMove board opponentSymbol

-- Ordem preferida das células: centro(4), cantos(0,2,6,8), bordas(1,3,5,7)
preferredCells :: [Int]
preferredCells = [4,0,2,6,8,1,3,5,7]

-- Escolhe a primeira célula vazia pela ordem preferida
pickBestEmptyCell :: [String] -> Maybe Int
pickBestEmptyCell board =
  listToMaybe [idx | idx <- preferredCells, cellAt board idx == ' ']

-- Obtém a lista de quadrantes disponíveis
availableQuadrants :: [Maybe Char] -> Maybe Int -> [Int]
availableQuadrants winnerBoard maybeNextQuadrant =
  case maybeNextQuadrant of
    Just q  -> [q | winnerBoard !! q == Nothing]
    Nothing -> [i | i <- [0..8], winnerBoard !! i == Nothing]

-- Pontua o quadrante: atualmente simples, +10 se o bot puder ganhar o tabuleiro grande com ele,
-- +5 se bloquear a vitória do oponente no tabuleiro grande, senão 1
-- Você pode expandir isso depois para ser mais estratégico
scoreQuadrant :: [Maybe Char] -> Char -> Char -> Int -> Int
scoreQuadrant winnerBoard botSymbol opponentSymbol quadrant =
  let
    -- Auxiliares para checar linhas de 3 no tabuleiro grande de vencedores
    bigWinningLines = 
      [ [0,1,2], [3,4,5], [6,7,8]
      , [0,3,6], [1,4,7], [2,5,8]
      , [0,4,8], [2,4,6]
      ]
    -- Verifica se colocar botSymbol no quadrante leva a 3 em linha no tabuleiro grande
    wouldWin = any (\line -> all (\i -> i == quadrant || winnerBoard !! i == Just botSymbol) line) bigWinningLines
    -- Verifica se o oponente está prestes a ganhar o tabuleiro grande colocando no quadrante
    opponentThreat = any (\line -> all (\i -> i == quadrant || winnerBoard !! i == Just opponentSymbol) line) bigWinningLines
  in
    if winnerBoard !! quadrant /= Nothing then 0
    else if wouldWin then 10
    else if opponentThreat then 5
    else 1

-- Escolhe o melhor quadrante baseado na pontuação e disponibilidade
chooseBestQuadrant :: [Maybe Char] -> Char -> Char -> Maybe Int -> Int
chooseBestQuadrant winnerBoard botSymbol opponentSymbol maybeNextQuadrant =
  let quads = availableQuadrants winnerBoard maybeNextQuadrant
      scored = [(q, scoreQuadrant winnerBoard botSymbol opponentSymbol q) | q <- quads]
  in fst $ maximumBy (comparing snd) scored

-- Função principal de jogada do bot
botTakeTurn
  :: [String]         -- Tabuleiro grande
  -> [[String]]       -- Tabuleiros pequenos
  -> Char             -- Símbolo do bot
  -> Maybe Int        -- Quadrante forçado (se houver)
  -> [Maybe Char]     -- Tabuleiro de vencedores
  -> IO (Int, [String])
botTakeTurn bigBoard smallBoards botSymbol maybeNextQuadrant winnerBoard = do
    putStrLn "Bot está pensando..."
    threadDelay (2 * 1000000)  -- pausa de 2s

    let opponentSymbol = if botSymbol == 'X' then 'O' else 'X'

    -- Escolhe o melhor quadrante para jogar
    let chosenQuadrant = chooseBestQuadrant winnerBoard botSymbol opponentSymbol maybeNextQuadrant
    let currentSmall = smallBoards !! chosenQuadrant

    -- Tenta encontrar jogada vencedora para o bot no tabuleiro pequeno
    let moveWin = findWinningMove currentSmall botSymbol

    -- Tenta bloquear jogada vencedora do oponente se não houver vitória imediata
    let moveBlock = if moveWin == Nothing then findBlockingMove currentSmall botSymbol opponentSymbol else Nothing

    -- Caso contrário, escolhe a melhor célula vazia pela preferência
    let moveCell = case moveWin of
                      Just idx -> Just idx
                      Nothing -> case moveBlock of
                                   Just idx -> Just idx
                                   Nothing -> pickBestEmptyCell currentSmall

    case moveCell of
      Just idx -> do
        let newSmall = placeSymbol currentSmall idx botSymbol
        return (chosenQuadrant, newSmall)
      Nothing -> do
        -- Nenhuma jogada encontrada, fallback para a primeira célula vazia (não deveria acontecer)
        let fallbackIdx = head [i | i <- [0..8], cellAt currentSmall i == ' ']
        let newSmall = placeSymbol currentSmall fallbackIdx botSymbol
        return (chosenQuadrant, newSmall)
