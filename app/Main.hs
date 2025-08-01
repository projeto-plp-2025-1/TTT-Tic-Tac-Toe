module Main where

import Interface.Tabuleiro (gameLoop)
import Interface.Menu (exibirMenu)
import Regras (exibirRegras)
import System.IO (readFile)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    opcao <- exibirMenu
    case opcao of
        "1" -> iniciarJogo
        "2" -> exibirRegras >> main
        "3" -> sairDoJogo
        _   -> do
            putStrLn "--- Opção inválida. Tente novamente. ---"
            main

iniciarJogo :: IO ()
iniciarJogo = do
    putStrLn "\n--- Iniciando novo jogo ---"
    templateContent <- readFile "assets/board_template_jogada.txt"
    let initialBoard = lines templateContent
    let startingPlayer = 'X'
    gameLoop initialBoard startingPlayer
    main  


sairDoJogo :: IO ()
sairDoJogo = do
    putStrLn "\nAté a próxima!"
    exitSuccess

