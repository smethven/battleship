module Battleship where

-- Battleship are two 10*10 grids, x-axis: A-J, y-axis: 0-9

-- One grid is your own battleships described by:
-- location of ships
-- location of hits on your ships

-- Other grid is opponent battleships:
-- location of sunk ships
-- location of hits of your guesses
-- location of misses of your guesses

-- ships cover squares on the grid
-- how many ships and of what size?
-- 1 carrier: 5 squares long
-- 1 battleship: 4 squares long
-- 1 cruiser: 3 squares long
-- 1 submarine: 3 squares long
-- 1 destroyer: 2 squares long

-- your move involves making a guess which would add to either hits or misses list of OpponentGrid, and if it hits then possibly adds to SunkShips list
-- if you hit then it also affects opponents own grid - adds to opponent's YourGrid misses

-- 1 init server game state and 1 init client game state

-- Well formed ship is straight and connected
-- straight
    -- Either XCoord or YCoord is all the same
-- connected
    -- Whichever is not the same must be able to be sorted
    -- and then ascending or descending without break
    -- e.g. C D E
    -- 5 6 7
-- E.g. Ship [Coord A 3, Coord A 5, Coord A 4]
-- Sorted by the changing type of YCoord
-- Ship [Coord A 3, Coord A 4, Coord A 5]
-- X Cord is repeating and YCord is contiguous (no repeats)

-- no repeats maybe a set would be better


-- GameState BattleshipState BoardView
data GameState = GameState BattleshipState BoardView

-- BattleshipState YourState [PreviousAttacks]
data BattleshipState = BSS PlayerState [GridSquare]

-- YourGrid [Ships] [Hits]
data PlayerState = PlayerState [Ship] [GridSquare]

-- a well-formed ship has all gridsquares in a line
data Ship = Ship [GridSquare]

data GridSquare = Square XCoord YCoord
  deriving (Eq, Show)

data XCoord = A | B | C | D | E | F | G | H | I | J
  deriving (Bounded, Enum, Show, Eq, Ord)
data YCoord = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Bounded, Enum, Show, Eq, Ord)

-- BV YourBoard OpponentBoard
data BoardView = BV [[String]] [[String]]

data Attack = GridSquare

-- Response Attacked Hit SunkShip WonGame
data Response = Response GridSquare Bool Bool Bool

data UpdateType = ShipSquare | Hit | Miss

-- Constants
serverStartState :: GameState
serverStartState = GameState serverBattleshipState serverBoardView

serverBattleshipState :: BattleshipState
serverBattleshipState = BSS serverPlayerState []

serverPlayerState :: PlayerState
serverPlayerState = PlayerState [] [] -- TODO stub

serverBoardView :: BoardView
serverBoardView = BV (buildNewBoard serverPlayerState) emptyBoard

clientStartState :: GameState
clientStartState = GameState clientBattleshipState clientBoardView

clientBattleshipState :: BattleshipState
clientBattleshipState = BSS clientPlayerState []

clientPlayerState :: PlayerState
-- clientPlayerState = PlayerState [] [] -- TODO stub
clientPlayerState = PlayerState [Ship [Square A One, Square A Two, Square A Three],
                                 Ship [Square C One, Square D One, Square E One],
                                 Ship [Square J Five, Square I Five]] [] -- stub

clientBoardView :: BoardView
clientBoardView = BV (buildNewBoard clientPlayerState) emptyBoard

emptyBoard :: [[String]]
emptyBoard = replicate 10 (replicate 10 "~")

boardWidth :: Int
boardWidth = 21

boardSpacerSize :: Int
boardSpacerSize = 5

headerSpacerSize :: Int
headerSpacerSize = boardSpacerSize + 16

boardViewHeader :: String
boardViewHeader = "Your Board:" ++ replicate headerSpacerSize ' ' ++ "Opponent's Board:"

boardHeader :: String
boardHeader = unwords $ spacer ++ header ++ replicate boardSpacerSize " " ++ spacer ++ header
  where header = map show [A ..J]
        spacer = [" "]

totalShipLength :: Int
totalShipLength = 17


-- Functions
wonGame :: GameState -> Bool
wonGame (GameState (BSS playerState attacks) bv) = length hits == totalShipLength
  where (PlayerState ships hits) = playerState

updateOnResponse :: Response -> GameState -> GameState
updateOnResponse (Response square hit sunk won) (GameState bss bv) =
  GameState (updateBSSOnResponse square bss) (updateBVOnRepsonse square hit bv)

updateBSSOnResponse :: GridSquare -> BattleshipState -> BattleshipState
updateBSSOnResponse (Square x y) (BSS state attacks) = BSS state (Square x y:attacks)

updateBVOnRepsonse :: GridSquare -> Bool -> BoardView -> BoardView
updateBVOnRepsonse square hit (BV playerBoard oppBoard) =
  BV playerBoard (updateBoard square update oppBoard)
    where  update = if hit then Hit else Miss

updateBoard :: GridSquare -> UpdateType -> [[String]] -> [[String]]
updateBoard _ _ [] = []
updateBoard (Square xCoord Zero) update (l:lines) = updateLine x update l:lines
  where x = fromEnum xCoord
updateBoard (Square xCoord yCoord) update (l:lines) =
  l:updateBoard (Square xCoord (pred yCoord)) update lines

updateLine :: Int -> UpdateType -> [String] -> [String]
updateLine _ _ [] = []
updateLine 0 Hit (_:rest) = "X":rest
updateLine 0 Miss (_:rest) = "O":rest
updateLine 0 ShipSquare (_:rest) = "S":rest
updateLine x hit (l:rest) = l:updateLine (x-1) hit rest

buildNewBoard :: PlayerState -> [[String]]
buildNewBoard (PlayerState ships _) = buildOnBoard ships emptyBoard
  where buildOnBoard [] board = board
        buildOnBoard (ship:ships) board = createShip ship (buildOnBoard ships board)

createShip :: Ship -> [[String]] -> [[String]]
createShip (Ship []) board = board
createShip (Ship (sqr:sqrs)) lines = updateBoard sqr ShipSquare updatedLines
  where updatedLines = createShip (Ship sqrs) lines

showBoards :: BoardView -> String
showBoards (BV player opponent) = unlines [boardViewHeader, boardHeader,
                                           combineLines Zero player opponent,
                                           combineLines One player opponent,
                                           combineLines Two player opponent,
                                           combineLines Three player opponent,
                                           combineLines Four player opponent,
                                           combineLines Five player opponent,
                                           combineLines Six player opponent,
                                           combineLines Seven player opponent,
                                           combineLines Eight player opponent,
                                           combineLines Nine player opponent]

combineLines :: YCoord -> [[String]] -> [[String]] -> String
combineLines y playerLines oppLines =
  unwords $ head ++ playerLine ++ replicate boardSpacerSize " " ++ head ++ opponentLine
  where head = [show (fromEnum y)]
        coord = fromEnum y
        playerLine = playerLines!!coord
        opponentLine = oppLines!!coord


testBoardView :: BoardView
testBoardView = BV testBoard testBoard

testBoard :: [[String]]
testBoard = [["S", "~", "S", "S", "S", "~", "~", "~", "~", "~"],
             ["S", "~", "~", "~", "~", "~", "X", "~", "~", "~"],
             ["S", "~", "~", "~", "~", "~", "X", "~", "~", "~"],
             ["S", "~", "~", "~", "~", "~", "X", "~", "~", "~"],
             ["~", "~", "~", "~", "~", "~", "X", "~", "~", "~"],
             ["~", "~", "~", "~", "X", "X", "X", "~", "~", "~"],
             ["~", "~", "~", "~", "~", "~", "~", "~", "~", "~"],
             ["~", "~", "~", "~", "~", "~", "~", "~", "~", "~"],
             ["~", "~", "~", "~", "~", "~", "~", "S", "S", "~"],
             ["~", "~", "~", "~", "~", "~", "~", "~", "~", "~"]]
