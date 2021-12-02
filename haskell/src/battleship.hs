-- Battleship are two 10*10 grids, x-axis: A-J, y-axis: N1-N10

-- One grid is your own battleships described by:
-- location of ships
-- location of hits on your ships

-- Other grid is opponent battleships: 
-- location of sunk ships
-- location of hits of your guesses 
-- location of misses of your guesses

-- ships cover squares on the grid
-- 1 carrier: 5 squares long
-- 1 battleship: 4 squares long
-- 1 cruiser: 3 squares long
-- 1 submarine: 3 squares long
-- 1 destroyer: 2 squares long

data BattleshipState = BattleshipState YourGrid OpponentGrid
    deriving (Eq, Ord, Read, Show)

-- YourGrid [Ships] [Hits]
data YourGrid = YourGrid [Ship] [GridSquare]
    deriving (Eq, Ord, Read, Show)

-- OpponentGrid [SunkShips] [Hits] [Misses]
data OpponentGrid = OpponentGrid [Ship] [GridSquare] [GridSquare]
    deriving (Eq, Ord, Read, Show)

-- a well-formed ship has all gridsquares in a line
data Ship = Ship [GridSquare]
    deriving (Eq, Ord, Read, Show)

data GridSquare = GridSquare XCoord YCoord
    deriving (Eq, Ord, Read, Show)
data XCoord = A | B | C | D | E | F | G | H | I | J
    deriving (Eq, Ord, Read, Bounded, Enum, Show)
data YCoord = N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10
    deriving (Eq, Ord, Read, Bounded, Enum, Show)

-- your move involves making a guess which would add to either hits or misses list of OpponentGrid, and if it hits then possibly adds to SunkShips list
-- if you hit then it also affects opponents own grid - adds to opponent's YourGrid misses

-- init server game state
initBattleshipServerState :: BattleshipState
initBattleshipServerState = BattleshipState initYourGridServer initOpponentGridServer

initYourGridServer :: YourGrid
initYourGridServer = YourGrid initShipsServer []

initShipsServer :: [Ship]
initShipsServer = [initCarrierServer, initBattleshipServer, initCruiserServer, initSubmarineServer, initDestroyerServer]

initCarrierServer :: Ship
initCarrierServer = Ship [(GridSquare B N2), (GridSquare C N2), (GridSquare D N2), (GridSquare E N2), (GridSquare F N2)]

initBattleshipServer :: Ship
initBattleshipServer = Ship [(GridSquare I N2), (GridSquare I N3), (GridSquare I N4), (GridSquare I N5)]

initCruiserServer :: Ship
initCruiserServer = Ship [(GridSquare C N4), (GridSquare D N4), (GridSquare E N4)]

initSubmarineServer :: Ship
initSubmarineServer = Ship [(GridSquare A N7), (GridSquare A N8), (GridSquare A N9)]

initDestroyerServer :: Ship
initDestroyerServer = Ship [(GridSquare H N10), (GridSquare I N10)]

initOpponentGridServer :: OpponentGrid
initOpponentGridServer = OpponentGrid [] [] []

-- init client game state
initBattleshipClientState :: BattleshipState
initBattleshipClientState = BattleshipState initYourGridClient initOpponentGridClient

initYourGridClient :: YourGrid
initYourGridClient = YourGrid initShipsClient []

initShipsClient :: [Ship]
initShipsClient = [initCarrierClient, initBattleshipClient, initCruiserClient, initSubmarineClient, initDestroyerClient]

initCarrierClient :: Ship
initCarrierClient = Ship [(GridSquare C N1), (GridSquare C N2), (GridSquare C N3), (GridSquare C N4), (GridSquare C N5)]

initBattleshipClient :: Ship
initBattleshipClient = Ship [(GridSquare J N1), (GridSquare J N2), (GridSquare J N3), (GridSquare J N4)]

initCruiserClient :: Ship
initCruiserClient = Ship [(GridSquare A N7), (GridSquare B N7), (GridSquare C N7)]

initSubmarineClient :: Ship
initSubmarineClient = Ship [(GridSquare A N9), (GridSquare B N9), (GridSquare C N9)]

initDestroyerClient :: Ship
initDestroyerClient = Ship [(GridSquare G N6), (GridSquare H N6)]

initOpponentGridClient :: OpponentGrid
initOpponentGridClient = OpponentGrid [] [] []