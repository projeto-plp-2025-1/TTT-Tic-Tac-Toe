module Utils.Types where

data Player = X | O deriving (Show, Eq)
data QuadrantState = InProgress | Winner Player | Draw deriving (Show, Eq)
type Cell = Maybe Player
type MiniBoard = [[Cell]]
type GameBoard = [[MiniBoard]]

data GameState = GameState {
    board         :: GameBoard,
    quadrantStates :: [QuadrantState],
    currentPlayer  :: Player
}
