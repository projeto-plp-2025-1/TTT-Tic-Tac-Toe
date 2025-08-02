module Interface.Regras where

import Interface.Arte (clearScreen)

corAmarela :: String -> String
corAmarela texto = "\ESC[33m" ++ texto ++ "\ESC[0m"

corBranca :: String -> String
corBranca texto = "\ESC[37m" ++ texto ++ "\ESC[0m"

-- Exibe as regras completas do jogo e aguarda o usuário para voltar ao menu.
exibirRegras :: IO ()
exibirRegras = do
    clearScreen
    putStrLn "                                                                                                              "
    putStrLn $ "  " ++ corAmarela "                     REGRAS DO SUPER JOGO DA VELHA " ++ "                                "
    putStrLn "                                                                                                              "
    putStrLn $ "  " ++ corAmarela "ESTRUTURA DO JOGO" ++ "                                                                  "
    putStrLn "       - O jogo consiste em um tabuleiro principal 3x3, onde cada quadrante contém                            "
    putStrLn "         um tabuleiro menor 3x3.                                                                              "
    putStrLn "       - O total de células jogáveis é 81.                                                                    "
    putStrLn "                                                                                                              "
    putStrLn $ "  " ++ corAmarela "OBJETIVO" ++ "                                                                           "
    putStrLn "       - O objetivo é conquistar 3 quadrantes em linha, coluna ou diagonal.                                   "
    putStrLn "       - Um quadrante é conquistado ao vencer o jogo da velha menor.                                          "
    putStrLn "                                                                                                              "
    putStrLn $ "  " ++ corAmarela "COMO JOGAR (TURNO A TURNO)" ++ "                                                         "
    putStrLn "       - A primeira jogada pode ser feita em qualquer célula de qualquer quadrante.                           "   
    putStrLn "       - A posição da sua jogada determina para onde o oponente vai.                                          "
    putStrLn $ "     - EXCEÇÃO: Se o quadrante de destino já foi ganho ou está cheio, o jogador                             "
    putStrLn "         da vez pode escolher qualquer outro quadrante livre.                                                 "
    putStrLn "                                                                                                              "
    putStrLn $ "  " ++ corAmarela "CONDIÇÕES DE VITÓRIA E EMPATE ('VELHA')" ++ "                                            "
    putStrLn "       - Vence quem primeiro formar uma sequência de 3 quadrantes conquistados.                               "
    putStrLn "       - Se todos os quadrantes forem preenchidos sem um vencedor, ocorre uma 'Velha'.                        "
    putStrLn "       - Em caso de 'Velha', vence o jogador que tiver conquistado mais quadrantes.                           "
    putStrLn "       - Se houver empate na contagem, a 'Velha' é considerada a vencedora.                                   "
    putStrLn "                                                                                                              "
    putStrLn " Pressione Enter para voltar ao menu..."
    _ <- getLine 
    return ()