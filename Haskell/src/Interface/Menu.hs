module Interface.Menu where

import Interface.Arte (exibirInicio)

-- Exibe a tela de início e as opções do menu, retornando a escolha do usuário.
exibirMenu :: IO String
exibirMenu = do
    exibirInicio -- Mostra a arte da tela inicial.
    
    putStrLn "  1. Novo Jogo"   -- Jogadores inserem seus nomes e símbolos
    putStrLn "  2. Continuar Jogo" -- Nova opção
    putStrLn "  3. Ver Regras"  -- Acesso ao manual de regras
    putStrLn "  4. Ver Ranking" -- Exibe os 5 jogadores com mais vitórias
    putStrLn "  5. Sair"        -- Encerra o jogo
    putStrLn ""
    putStr "Escolha uma opção: "
    putStrLn ""
    getLine
