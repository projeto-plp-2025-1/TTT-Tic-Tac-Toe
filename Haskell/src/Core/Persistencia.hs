{-# LANGUAGE ScopedTypeVariables #-}
module Core.Persistencia where

import System.Directory
import Data.List (sortOn)
import Data.Ord (Down(..))
import Utils.Types (Jogador(..))
import Data.Char (toUpper, isDigit)
import Control.Exception (handle, SomeException, catch)
import Control.Concurrent (threadDelay)

caminhoArquivo :: FilePath
caminhoArquivo = "dados/jogadores.txt"

-- Cria uma conta nova se ainda não existe
criarConta :: String -> IO ()
criarConta novoNome = atomicUpdate $ \jogadores ->
    if novoNome `elem` map nome jogadores
    then jogadores
    else Jogador novoNome 0 : jogadores

-- Garante que o nome seja único (case-insensitive)
nomeUnico :: String -> IO String
nomeUnico nomeBase = do
    jogadores <- carregarJogadores
    let nomesExistentes = map (map toUpper . nome) jogadores
    return $ gerarNomeUnico (map toUpper nomeBase) nomesExistentes 1

gerarNomeUnico :: String -> [String] -> Int -> String
gerarNomeUnico nomeExistentes existentes n
    | nomeExistentes `notElem` existentes = nomeExistentes
    | otherwise = gerarNomeUnico (base ++ show n) existentes (n + 1)
  where
    base = takeWhile (not . isDigit) nomeExistentes

-- Registra vitória de um jogador
registrarVitoria :: String -> IO ()
registrarVitoria "Bot" = putStr ""
registrarVitoria vencedorNome = atomicUpdate $ 
    incrementaJogador vencedorNome

-- Carrega jogadores salvos
carregarJogadores :: IO [Jogador]
carregarJogadores = handle fallback $ do
    existe <- doesFileExist caminhoArquivo
    if not existe then return [] else do
        conteudo <- readFile caminhoArquivo
        return $ map read (lines conteudo)
  where
    fallback :: SomeException -> IO [Jogador]
    fallback _ = return []


formatarJogador :: Jogador -> String
formatarJogador (Jogador nome vits) = show (Jogador nome vits)

-- Salva jogadores no arquivo
salvarJogadores :: [Jogador] -> IO ()
salvarJogadores jogadores = handle fallback $ do
    -- Cria arquivo temporário primeiro
    let tempPath = caminhoArquivo ++ ".tmp"
    writeFile tempPath (unlines $ map formatarJogador jogadores)
    -- Renomeia atomicamente
    renameFile tempPath caminhoArquivo `catch` (\(_::SomeException) -> removeFile tempPath)
  where
    fallback :: SomeException -> IO ()
    fallback e = putStrLn $ "Erro ao salvar jogadores: " ++ show e

-- Função auxiliar 
atomicUpdate :: ([Jogador] -> [Jogador]) -> IO ()
atomicUpdate updateFn = do
    createDirectoryIfMissing True "dados"
    jogadores <- carregarJogadores
    salvarJogadores (updateFn jogadores)
    where
        retry :: SomeException -> IO ()
        retry _ = threadDelay 100000 >> atomicUpdate updateFn


-- Retorna os 5 jogadores com mais vitórias
rankingTop5 :: IO [Jogador]
rankingTop5 = do
    jogadores <- carregarJogadores
    return $ take 5 (sortOn (Down . vitorias) jogadores)

-- Atualiza o número de vitórias do jogador
incrementaJogador :: String -> [Jogador] -> [Jogador]
incrementaJogador nomeNovo [] = [Jogador nomeNovo 1]
incrementaJogador nomeNovo (j:js)
    | nomeNovo == nome j = Jogador nomeNovo (vitorias j + 1) : js
    | otherwise           = j : incrementaJogador nomeNovo js

