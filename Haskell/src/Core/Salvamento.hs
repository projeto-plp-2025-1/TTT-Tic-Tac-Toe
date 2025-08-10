module Core.Salvamento where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.IO (withFile, IOMode(..), hPutStrLn, hGetContents)
import Data.List.Split (splitOn)
import Data.Char (toUpper)
import Utils.Types (QuadrantState(..), SmallBoardState, GameState(..), inicializaWinnerBoard)

-- Salvar jogo
salvarJogo :: GameState -> IO ()
salvarJogo saveData = do
    createDirectoryIfMissing True "dados"
    withFile "dados/salvo.txt" WriteMode $ \h -> do
        hPutStrLn h $ "jogador1;" ++ [fst $ jogador1 saveData] ++ ";" ++ snd (jogador1 saveData)
        hPutStrLn h $ "j1SmallWin;" ++ show (j1SmallWin saveData)
        hPutStrLn h $ "jogador2;" ++ [fst $ jogador2 saveData] ++ ";" ++ snd (jogador2 saveData)
        hPutStrLn h $ "j2SmallWin;" ++ show (j2SmallWin saveData)
        hPutStrLn h $ "vez;" ++ [vezAtual saveData]
        hPutStrLn h $ "quadrantePermitido;" ++ maybe "None" show (quadrante saveData)
        hPutStrLn h $ "winnerBoard;" ++ concatMap serializeWinnerBoard (winnerBoard saveData)
        hPutStrLn h $ "tabuleiroMaior;" ++ serializeLines (bigBoard saveData)
        hPutStrLn h $ "tabuleiroMenores;" ++ serializeMiniBoards (smallBoards saveData)

-- Carregar jogo
carregarJogo :: IO (Maybe GameState)
carregarJogo = do
    existe <- doesFileExist "dados/salvo.txt"
    if not existe then return Nothing else
        withFile "dados/salvo.txt" ReadMode $ \h -> do
            conteudo <- hGetContents h
            length conteudo `seq` return (parseSaveData (lines conteudo))
        
-- Serializadores auxiliares
serializeLines :: [String] -> String
serializeLines = concat . map (++ "|")

serializeMiniBoards :: [[String]] -> String
serializeMiniBoards = concat . map (\b -> serializeLines b ++ "::")

serializeWinnerBoard :: QuadrantState -> String
serializeWinnerBoard = format
  where
    format InProgress  = "InProgress,"
    format Draw        = "Draw,"
    format (Winner c)  = "Winner " ++ [c] ++ ","
    
-- Desserializadores
parseSaveData :: [String] -> Maybe GameState
parseSaveData ls = do
    j1Line <- lookupLine "jogador1" ls
    j1WinLine <- lookupLine "j1SmallWin" ls
    j2Line <- lookupLine "jogador2" ls
    j2WinLine <- lookupLine "j2SmallWin" ls
    vezLine <- lookupLine "vez" ls
    quadLine <- lookupLine "quadrantePermitido" ls
    winnerLine <- lookupLine "winnerBoard" ls
    bigLine <- lookupLine "tabuleiroMaior" ls
    miniLine <- lookupLine "tabuleiroMenores" ls

    let j1 = parseJogador j1Line
        j2 = parseJogador j2Line
        vez = head vezLine
        quad = if quadLine == "None" then Nothing else Just (read quadLine)
        winner = parseWinnerBoard winnerLine
        big = splitOn "|" bigLine
        minis = map (splitOn "|") (init $ splitOn "::" miniLine)
        
    j1SmallWin <- safeRead j1WinLine
    j2SmallWin <- safeRead j2WinLine
    
    return $ GameState j1 j1SmallWin j2 j2SmallWin vez quad (init big) minis winner
  where
    safeRead :: String -> Maybe Int
    safeRead s = case reads s of
                  [(x, "")] -> Just x
                  _ -> Nothing

lookupLine :: String -> [String] -> Maybe String
lookupLine prefix ls = case filter (prefix `startsWith`) ls of
    (l:_) -> Just (drop (length prefix + 1) l)
    _     -> Nothing

startsWith :: String -> String -> Bool
startsWith pre str = map toUpper pre == map toUpper (take (length pre) str)

parseJogador :: String -> (Char, String)
parseJogador s = case splitOn ";" s of
    [c,n] -> (head c, n)
    _     -> ('?', "")

parseWinnerBoard :: String -> [QuadrantState]
parseWinnerBoard input = 
    case filter (/= "") (splitOn "," input) of
        [] -> inicializaWinnerBoard
        items -> map parseItem items
  where
    parseItem "Draw" = Draw
    parseItem ['W','i','n','n','e','r',' ',c] = Winner c
    parseItem _ = InProgress