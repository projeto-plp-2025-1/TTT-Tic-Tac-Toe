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
        hPutStrLn h $ "player1;" ++ [fst $ player1 saveData] ++ ";" ++ snd (player1 saveData)
        hPutStrLn h $ "p1SmallWin;" ++ show (p1SmallWin saveData)
        hPutStrLn h $ "player2;" ++ [fst $ player2 saveData] ++ ";" ++ snd (player2 saveData)
        hPutStrLn h $ "p2SmallWin;" ++ show (p2SmallWin saveData)
        hPutStrLn h $ "vez;" ++ [curentPlayer saveData]
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
    p1Line <- lookupLine "player1" ls
    p1WinLine <- lookupLine "p1SmallWin" ls
    p2Line <- lookupLine "player2" ls
    p2WinLine <- lookupLine "p2SmallWin" ls
    vezLine <- lookupLine "vez" ls
    quadLine <- lookupLine "quadrantePermitido" ls
    winnerLine <- lookupLine "winnerBoard" ls
    bigLine <- lookupLine "tabuleiroMaior" ls
    miniLine <- lookupLine "tabuleiroMenores" ls

    let p1 = parseJogador p1Line
        p2 = parseJogador   p2Line
        vez = head vezLine
        quad = if quadLine == "None" then Nothing else Just (read quadLine)
        winner = parseWinnerBoard winnerLine
        big = splitOn "|" bigLine
        minis = map (splitOn "|") (init $ splitOn "::" miniLine)
        
    p1SmallWin <- safeRead p1WinLine
    p2SmallWin <- safeRead  p2WinLine
    
    return $ GameState p1 p1SmallWin    p2  p2SmallWin vez quad (init big) minis winner
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