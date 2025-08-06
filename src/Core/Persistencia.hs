module Core.Persistencia where

import System.Directory
import Data.List (sortOn)
import Data.Ord (Down(..))
import Utils.Types (Jogador(..))
import Data.Char (toUpper, isDigit)

caminhoArquivo :: FilePath
caminhoArquivo = "dados/jogadores.txt"

-- Cria uma conta nova se ainda não existe
criarConta :: String -> IO ()
criarConta novoNome = do
    createDirectoryIfMissing True "dados"
    jogadores <- carregarJogadores
    let nomes = map nome jogadores
    if novoNome `elem` nomes
        then return ()
        else salvarJogadores (Jogador novoNome 0 : jogadores)

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
registrarVitoria vencedorNome = do
    createDirectoryIfMissing True "dados"
    jogadores <- carregarJogadores
    let atualizados = incrementaJogador vencedorNome jogadores
    salvarJogadores atualizados

-- Carrega jogadores salvos
carregarJogadores :: IO [Jogador]
carregarJogadores = do
    existe <- doesFileExist caminhoArquivo
    if not existe then return [] else do
        conteudo <- readFile caminhoArquivo
        return $ map read (lines conteudo)

formatarJogador :: Jogador -> String
formatarJogador (Jogador nome vits) = show (Jogador nome vits)

-- Salva jogadores no arquivo
salvarJogadores :: [Jogador] -> IO ()
salvarJogadores jogadores =
    writeFile caminhoArquivo (unlines $ map formatarJogador jogadores)

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

