module Interface.Menu where

import Arte (exibirInicio)

-- Exibe a tela de início e as opções do menu, retornando a escolha do usuário.
exibirMenu :: IO String
exibirMenu = do
    exibirInicio -- Mostra a arte da tela inicial.
    
    putStrLn "  1. Novo Jogo" -- Ao iniciar o jogo, os jogadores devem inserir seus nomes, que serão exibidos durante a partida [cite: 215]
    putStrLn "  2. Ver Regras" -- No menu inicial, também é possível acessar o conjunto de regras e o manual do jogo. [cite: 220]
    putStrLn "  3. Sair" -- O jogo pode ser encerrado a qualquer momento por qualquer um dos jogadores. [cite: 231]
    putStrLn ""
    putStr "Escolha uma opção: "
    getLine