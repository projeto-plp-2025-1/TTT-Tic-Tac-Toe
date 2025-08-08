module Utils.Types where

--data Player = char | notingh deriving (Show, Eq)
--type Cell = Maybe Player
--type MiniBoard = [[Cell]]
--type GameBoard = [[MiniBoard]]

data Jogador = Jogador {
    nome :: String,
    vitorias :: Int
} deriving (Show, Read)

data QuadrantState = InProgress | Winner Char | Draw
  deriving (Show, Eq)

-- Status de small board (9 quadrantes)
type SmallBoardState = [QuadrantState]

-- Inicializa todos os quadrantes como "InProgress"
inicializaWinnerBoard :: [QuadrantState]
inicializaWinnerBoard = replicate 9 InProgress

data GameState = GameState{ 
    jogador1     :: (Char, String),
    jogador2     :: (Char, String),
    vezAtual     :: Char,
    quadrante    :: Maybe Int,
    bigBoard     :: [String],
    smallBoards  :: [[String]],
    winnerBoard  :: SmallBoardState
  } deriving Show
