module Main where

import Types
import Tabuleiro (desenharTabuleiro)
import Menu (exibirMenu)
import Arte (exibirVencedor)

-- Cria o estado inicial para um novo jogo
criarTabuleiroInicial :: GameBoard
criarTabuleiroInicial = replicate 3 (replicate 3 (replicate 3 (replicate 3 Nothing)))

estadoInicial :: GameState
estadoInicial = GameState {
    board = criarTabuleiroInicial,
    quadrantStates = replicate 9 InProgress,
    currentPlayer = X
}

-- Função principal que controla o fluxo do programa
main :: IO ()
main = do
    escolha <- exibirMenu
    case escolha of
        "1" -> gameLoop estadoInicial -- Inicia um novo jogo [cite: 214]
        "2" -> putStrLn "Regras ainda a serem implementadas." >> main
        "3" -> putStrLn "Até mais!"
        _   -> putStrLn "Opção inválida." >> main

-- O loop principal do jogo (ainda em desenvolvimento)
gameLoop :: GameState -> IO ()
gameLoop currentState = do
    desenharTabuleiro currentState
    putStrLn "\nJogo em andamento... (Pressione Enter para voltar ao menu)"
    _ <- getLine
    main