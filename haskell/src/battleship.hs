-- Battleship are two 10*10 grids, x-axis: A-J, y-axis: 1-10

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

-- data BattleshipState = BattleshipState YourGrid OpponentGrid

-- YourGrid [Ships] [Hits]
-- data YourGrid = YourGrid [Ship] [GridSquare]

-- OpponentGrid [SunkShips] [Hits] [Misses]
-- data OpponentGrid = OpponentGrid [Ship] [GridSquare] [GridSquare]

-- a well-formed ship has all gridsquares in a line
-- data Ship = Ship [GridSquare]

-- data GridSquare = GridSquare XCoord YCoord

-- data XCoord = A | B | C | D | E | F | G | H | I | J
--  deriving (Bounded, Enum, Show)
-- data YCoord = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10
--  deriving (Bounded, Enum, Show)

-- your move involves making a guess which would add to either hits or misses list of OpponentGrid, and if it hits then possibly adds to SunkShips list
-- if you hit then it also affects opponents own grid - adds to opponent's YourGrid misses

-- 1 init server game state and 1 init client game state

-- module Lib ( BattleshipState ) where
--   Grid =
--   type Coord = (Int, Int)

data Ship = Ship [GridSquare]
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

data GridSquare = GridSquare XCoord YCoord

data XCoord = A | B | C | D | E | F | G | H | I | J
  deriving (Bounded, Enum, Show)
data YCoord = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10
  deriving (Bounded, Enum, Show)
