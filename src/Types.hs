module Types where

data Player = X | O deriving (Show, Eq)
data QuadrantState = InProgress | Winner Player | Draw deriving (Show, Eq)
type Cell = Maybe Player
type MiniBoard = [[Cell]]
type GameBoard = [[MiniBoard]]

-- O estado completo do jogo, que será usado por todas as partes do programa
data GameState = GameState {
    board         :: GameBoard,
    quadrantStates :: [QuadrantState],
    currentPlayer  :: Player
}
