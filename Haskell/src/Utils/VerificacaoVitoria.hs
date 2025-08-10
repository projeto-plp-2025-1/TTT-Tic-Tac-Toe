module Utils.VerificacaoVitoria where

verificarVitoria :: [String] -> Char -> Bool
verificarVitoria board jogador =
    any (linhaCompleta board jogador) combinacoesVitoria

combinacoesVitoria :: [[(Int, Int)]]
combinacoesVitoria =
  [ [(2,5), (2,10), (2,14)]
  , [(4,5), (4,10), (4,14)]
  , [(6,5), (6,10), (6,14)]
  , [(2,5), (4,5), (6,5)]
  , [(2,10), (4,10), (6,10)]
  , [(2,14), (4,14), (6,14)]
  , [(2,5), (4,10), (6,14)]
  , [(2,14), (4,10), (6,5)]
  ]

linhaCompleta :: [String] -> Char -> [(Int, Int)] -> Bool
linhaCompleta board jogador indices =
    all (posicaoDoJogador board jogador) indices

posicaoDoJogador :: [String] -> Char -> (Int, Int) -> Bool
posicaoDoJogador board jogador (linha, coluna) =
    (board !! linha) !! coluna == jogador