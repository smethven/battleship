module Gameplay where

import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Text.Read (readMaybe)
import Gamestate
    ( Attack,
      Response(..),
      GameState(..),
      notPrevAttack,
      makeResponseFromAttack,
      updateOnAttack,
      updateOnResponse,
      showBoards,
      parseAttack )

-- The printout at the start of the game
gameStartPrintout :: IO ()
gameStartPrintout = putStrLn "Captain! An enemy fleet has appeared within striking distance!"

-- Read the attack that the connection sends and update the game state accordingly
getAttack :: Socket -> GameState -> IO ()
getAttack s gs = do
  putStrLn "The enemy is attacking!" >> putStrLn "..."
  msg <- recv s 1024
  let mAttack = readMaybe (C.unpack msg) :: Maybe Attack
  getAttackHelper s gs mAttack

getAttackHelper :: Socket -> GameState -> Maybe Attack -> IO ()
getAttackHelper _ _ Nothing =
  putStrLn "Error: Recieved invalid attack from the other player. Closing connection."
getAttackHelper s gs (Just attack) = do
  let gameResponse = makeResponseFromAttack attack gs
  let gs1 = updateOnAttack attack gs
  displayBoards gs1
  handleAttack s gs1 gameResponse

-- Print result of connection's attack and make decisions about game continuation
handleAttack :: Socket -> GameState -> Response -> IO ()
handleAttack _ _ (Response _ _ _ True) = do
  putStrLn "Hit! They sunk our last ship! We're going down!"
  putStrLn "You lost...!"
  putStrLn "GAME OVER"
handleAttack s gs response = do
  tellResponse response
  sendAll s (C.pack (show response))
  sendAttack s gs
  where tellResponse (Response  _ _ True _) = putStrLn "Hit! They sunk our ship!"
        tellResponse (Response  _ True _ _) = putStrLn "They hit our ship!"
        tellResponse (Response _ False _ _) = putStrLn "They missed! Phew!"

-- Read attack that user supplies and send it to the other connection
sendAttack :: Socket -> GameState -> IO ()
sendAttack s gs = do
  displayBoards gs
  putStrLn "Where do we fire, Captain? (Type a letter and a number, separated by a space)"
  msg <- getLine
  let mAttack = parseAttack msg
  sendAttackHelper s gs mAttack

sendAttackHelper :: Socket -> GameState -> Maybe Attack -> IO ()
sendAttackHelper s gs Nothing = do
  putStrLn "We cannot attack there, Captain! Pick another target!"
  sendAttack s gs
sendAttackHelper s gs (Just attack)
  | notPrevAttack attack gs = do
    putStrLn "Firing!"
    sendAll s (C.pack (show attack))
    getResponse s gs
  | otherwise = do
    putStrLn "We already fired on that location, Captain! Pick another target!"
    sendAttack s gs

-- Read the connection's response to the sent attack
getResponse :: Socket -> GameState -> IO ()
getResponse s gs = do
  msg <- recv s 1024
  let mResponse = readMaybe (C.unpack msg) :: Maybe Response
  getResponseHelper s gs mResponse

getResponseHelper :: Socket -> GameState -> Maybe Response -> IO ()
getResponseHelper _ _ Nothing =
  putStrLn "Error: Recieved invalid response from the other player. Closing connection."
getResponseHelper s gs (Just response) = do
  let gs1 = updateOnResponse response gs
  displayBoards gs1
  handleResponse s gs1 response

-- Print result of your attack and make game decisions about game continuation
handleResponse :: Socket -> GameState -> Response -> IO ()
handleResponse _ _ (Response _ _ _ True) = do
  putStrLn "Hit! You sunk the last ship!"
  putStrLn "You won!"
  putStrLn "GAME OVER"
handleResponse s gs response = do
  tellResponse response
  getAttack s gs
  where tellResponse (Response  _ _ True _) = putStrLn "Hit! You sunk a ship!"
        tellResponse (Response  _ True _ _) = putStrLn "That's a hit!"
        tellResponse (Response _ False _ _) = putStrLn "That's a miss. We'll get 'em next time!"

displayBoards :: GameState -> IO ()
displayBoards (GameState bss bv) = putStrLn (showBoards bv)
