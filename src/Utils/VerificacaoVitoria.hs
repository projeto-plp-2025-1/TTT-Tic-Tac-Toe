module Utils.VerificacaoVitoria where

verificarVitoria :: [String] -> Char -> Bool
verificarVitoria board jogador =
    any (linhaCompleta board jogador) combinacoesVitoria

combinacoesVitoria :: [[(Int, Int)]]
combinacoesVitoria =
  [ [(2,3), (2,8), (2,13)]
  , [(4,3), (4,8), (4,13)]
  , [(6,3), (6,8), (6,13)]
  , [(2,3), (4,3), (6,3)]
  , [(2,8), (4,8), (6,8)]
  , [(2,13), (4,13), (6,13)]
  , [(2,3), (4,8), (6,13)]
  , [(2,13), (4,8), (6,3)]
  ]

linhaCompleta :: [String] -> Char -> [(Int, Int)] -> Bool
linhaCompleta board jogador indices =
    all (posicaoDoJogador board jogador) indices

posicaoDoJogador :: [String] -> Char -> (Int, Int) -> Bool
posicaoDoJogador board jogador (linha, coluna) =
    (board !! linha) !! coluna == jogador

