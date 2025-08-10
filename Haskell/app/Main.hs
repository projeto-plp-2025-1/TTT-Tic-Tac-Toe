module Main where

import System.Process (callCommand)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Info (os)
import Utils.Types (inicializaWinnerBoard)
import System.IO (hFlush, stdout)
import Data.Char (toUpper)
import Interface.Arte (clearScreen, exibirInicio)
import Interface.Regras (exibirRegras)
import Interface.Menu (exibirMenu)
import Core.TabuleiroMaior (gameLoop)
import Core.TabuleiroMenor (
    smallBoard1Template, smallBoard2Template, smallBoard3Template,
    smallBoard4Template, smallBoard5Template, smallBoard6Template,
    smallBoard7Template, smallBoard8Template, smallBoard9Template
    )
import qualified Core.Persistencia as P
import qualified Core.Salvamento as S
import qualified Utils.Types as T

-- Compatibilidade com Windows e Linux
setUtf8EncodingCompat :: IO ()
setUtf8EncodingCompat =
    if os == "mingw32"
        then callCommand "chcp 65001 > nul"
        else return ()

-- Lista de personagens disponíveis: número, símbolo, nome padrão
personagens :: [(Int, Char, String)]
personagens =
    [ (1, 'X', "Xis")
    , (2, 'O', "Bola")
    , (3, '#', "Cerquinha")
    , (4, '@', "Arroba")
    , (5, '&', "E-Comercial")
    , (6, '%', "Porcento")
    , (7, 'Δ', "Delta")
    , (8, '☺', "Sorriso")
    , (9, '♣', "Trevo")
    ]

-- Mostrar personagens disponíveis
mostrarPersonagens :: [(Int, Char, String)] -> IO ()
mostrarPersonagens ps = do
    putStrLn "Escolha um personagem pelo número:"
    mapM_ (\(i, s, n) -> putStrLn $ show i ++ " -> [" ++ [s] ++ "] " ++ n) ps

-- Pedir escolha de personagem (símbolo + nome)
escolherPersonagem :: [Int] -> IO (Char, String)
escolherPersonagem usados = do
    let disponiveis = filter (\(i, _, _) -> i `notElem` usados) personagens
    mostrarPersonagens disponiveis
    
    putStr "Digite o número do personagem desejado (ou Enter para X/O): "
    hFlush stdout
    input <- getLine
    
    case input of
        "" -> do  -- Quando o usuário pressiona Enter
            let defaultSymbols = ['X', 'O']
                availableSymbols = [s | (_, s, _) <- disponiveis]
                symbol = head $ filter (`elem` availableSymbols) defaultSymbols
                
            case lookupSymbol symbol disponiveis of
                Just (s, nomePadrao) -> getPlayerName s nomePadrao
                Nothing -> erro
                
        _ -> case reads input :: [(Int, String)] of
            [(num, "")] ->
                case lookupPersonagem num disponiveis of
                Just (s, nomePadrao) -> getPlayerName s nomePadrao
                Nothing -> erro
            _ -> erro
  where
    -- Função auxiliar para obter o nome do jogador
    getPlayerName :: Char -> String -> IO (Char, String)
    getPlayerName s nomePadrao = do
        putStr $ "Deseja um nome personalizado para o personagem [" ++ [s] ++ "]? (pressione Enter para usar '" ++ nomePadrao ++ "'): "
        hFlush stdout
        nomeInput <- getLine
        let nomeBruto = if null nomeInput then nomePadrao else nomeInput
        nomeFinal <- P.nomeUnico nomeBruto
        return (s, nomeFinal)
    
    lookupPersonagem n = fmap (\(_, s, nome) -> (s, nome)) . safeHead . filter (\(i, _, _) -> i == n)
    lookupSymbol sym = fmap (\(_, s, nome) -> (s, nome)) . safeHead . filter (\(_, s, _) -> s == sym)
    safeHead [] = Nothing
    safeHead (x:_) = Just x
    erro = do
        putStrLn "Escolha inválida. Tente novamente."
        escolherPersonagem usados

-- Função principal
main :: IO ()
main = do
    setUtf8EncodingCompat
    setLocaleEncoding utf8
    clearScreen
    opcao <- exibirMenu
    case opcao of
        "1" -> escolherTipoDeJogo
        "2" -> continuarJogo
        "3" -> exibirRegras >> main
        "4" -> verRanking >> main
        "5" -> sairDoJogo
        _   -> main

verRanking :: IO ()
verRanking = do
    clearScreen
    putStrLn "==========================="
    putStrLn "      RANKING TOP 5"
    putStrLn "===========================\n"
    ranking <- P.rankingTop5
    if null ranking
        then putStrLn "Nenhum jogador registrado ainda.\n"
        else do
            mapM_
              (\(i, jogador) ->
                  putStrLn $ show i ++ ". " ++ T.nome jogador ++ " - " ++ show (T.vitorias jogador) ++ " vitórias")
              (zip [1..] ranking)
    putStrLn "\nPressione ENTER para voltar ao menu."
    _ <- getLine
    return ()

continuarJogo :: IO ()
continuarJogo = do
    jogoSalvo <- S.carregarJogo
    case jogoSalvo of
        Nothing -> do
            putStrLn "Nenhum jogo salvo encontrado."
            putStrLn "Pressione ENTER para voltar ao menu."
            _ <- getLine
            main
        Just save -> do
            let nome1 = snd (T.jogador1 save)
            let nome2 = snd (T.jogador2 save)
            let simbolo1 = fst (T.jogador1 save)
            let simbolo2 = fst (T.jogador2 save)
            let j1SmallWin = T.j1SmallWin save
            let j2SmallWin = T.j2SmallWin save
            let vez = T.vezAtual save
            let quad =T.quadrante save
            let bigBoard =T.bigBoard save
            let miniBoards =T.smallBoards save
            let winnerBoard =T.winnerBoard save

            putStrLn $ "Jogo salvo entre " ++ nome1 ++ " e " ++ nome2 ++ " carregado com sucesso!"
            putStrLn "Pressione ENTER para continuar o jogo..."
            _ <- getLine

            gameLoop bigBoard miniBoards simbolo1 simbolo2 vez quad nome1 nome2 j1SmallWin j2SmallWin winnerBoard


escolherTipoDeJogo :: IO ()
escolherTipoDeJogo = do
    putStrLn "\nEscolha o modo de jogo:"
    putStrLn "1 -> Multiplayer (2 jogadores locais)"
    putStrLn "2 -> Singleplayer (Contra o bot)"
    putStr "→ "
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> iniciarJogo
        "2" -> iniciarJogoContraBot
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            escolherTipoDeJogo

-- Início do jogo com personagens personalizados
iniciarJogo :: IO ()
iniciarJogo = do
    putStrLn "JOGADOR 1"
    jogador1 <- escolherPersonagem []
    P.criarConta (snd jogador1)
    putStrLn $ "Jogador 1 escolheu: [" ++ [fst jogador1] ++ "] " ++ snd jogador1

    putStrLn "\nJOGADOR 2"
    jogador2 <- escolherPersonagem [getId (fst jogador1)]
    P.criarConta (snd jogador2)
    putStrLn $ "Jogador 2 escolheu: [" ++ [fst jogador2] ++ "] " ++ snd jogador2

    putStrLn "\nPressione Enter para começar..."
    _ <- getLine

    iniciarPartida jogador1 jogador2

iniciarJogoContraBot :: IO ()
iniciarJogoContraBot = do
    putStrLn "VOCÊ"
    jogador1@(symbol1, name1) <- escolherPersonagem []
    P.criarConta name1
    putStrLn $ "Você escolheu: [" ++ [symbol1] ++ "] " ++ name1

    let symbol2 = head [s | (i, s, _) <- personagens, s /= symbol1]
    let name2 = "Bot"
    let jogador2 = (symbol2, name2)

    putStrLn $ "\nO bot jogará com: [" ++ [symbol2] ++ "] " ++ name2
    putStrLn "\nPressione Enter para começar..."
    _ <- getLine

    iniciarPartida jogador1 jogador2

iniciarPartida :: (Char, String) -> (Char, String) -> IO ()
iniciarPartida (symbol1, name1) (symbol2, name2) = do
    let initialBoard = -- Template do tabuleiro maior
                        [
                        "    1    2    3     4    5    6     7    8    9",
                        "                                                ",
                        "       │    │    ║    │    │    ║    │    │      A",
                        "   ────┼────┼────╢────┼────┼────╢────┼────┼──── ",
                        "       │    │    ║    │    │    ║    │    │      B",
                        "   ────┼────┼────╢────┼────┼────╢────┼────┼──── ",
                        "       │    │    ║    │    │    ║    │    │      C",
                        "     QUADRANTE 1 ║  QUADRANTE 2 ║  QUADRANTE 3  ",
                        "   ══════════════╬══════════════╬══════════════ ",
                        "       │    │    ║    │    │    ║    │    │      D",
                        "   ────┼────┼────╢────┼────┼────╢────┼────┼──── ",
                        "       │    │    ║    │    │    ║    │    │      E",
                        "   ────┼────┼────╢────┼────┼────╢────┼────┼──── ",
                        "       │    │    ║    │    │    ║    │    │      F",
                        "     QUADRANTE 4 ║  QUADRANTE 5 ║  QUADRANTE 6  ",
                        "   ══════════════╬══════════════╬══════════════ ",
                        "       │    │    ║    │    │    ║    │    │      G",
                        "   ────┼────┼────╢────┼────┼────╢────┼────┼──── ",
                        "       │    │    ║    │    │    ║    │    │      H",
                        "   ────┼────┼────╢────┼────┼────╢────┼────┼──── ",
                        "       │    │    ║    │    │    ║    │    │      I",
                        "     QUADRANTE 7 ║  QUADRANTE 8 ║  QUADRANTE 9  ",
                        "                                                "
                        ]

    let initialSmallBoards =
                    [ smallBoard1Template, smallBoard2Template, smallBoard3Template
                    , smallBoard4Template, smallBoard5Template, smallBoard6Template
                    , smallBoard7Template, smallBoard8Template, smallBoard9Template
                    ]

    let j1SmallWin = 0
    let j2SmallWin = 0
    let winnerBoard = inicializaWinnerBoard

    gameLoop initialBoard initialSmallBoards symbol1 symbol2 symbol1 Nothing name1 name2 j1SmallWin j2SmallWin winnerBoard


-- Função auxiliar: obter o ID de um símbolo (para evitar repetição)
getId :: Char -> Int
getId c = case lookup c (map (\(i, s, _) -> (s, i)) personagens) of
    Just i -> i
    Nothing -> -1

-- Sair do jogo
sairDoJogo :: IO ()
sairDoJogo = putStrLn "Saindo do jogo. Até mais!"
