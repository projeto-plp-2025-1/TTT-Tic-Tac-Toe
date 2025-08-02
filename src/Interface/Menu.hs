module Interface.Menu where

import Interface.Arte (exibirInicio)

-- Exibe a tela de início e as opções do menu, retornando a escolha do usuário.
exibirMenu :: IO String
exibirMenu = do
    exibirInicio -- Mostra a arte da tela inicial.
    
    putStrLn "  1. Novo Jogo" -- Ao iniciar o jogo, os jogadores devem inserir seus nomes, que serão exibidos durante a partida 
    putStrLn "  2. Ver Regras" -- No menu inicial, também é possível acessar o conjunto de regras e o manual do jogo.
    putStrLn "  3. Sair" -- O jogo pode ser encerrado a qualquer momento por qualquer um dos jogadores.
    putStrLn ""
    putStr "Escolha uma opção: "
    getLine