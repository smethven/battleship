module Gameplay where

import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Text.Read (readMaybe)
import Gamestate
    ( Attack,
      Response(..),
      GameState(..),
      GridSquare(..),
      notPrevAttack,
      makeResponseFromAttack,
      updateOnAttack,
      updateOnResponse,
      showBoards,
      parseAttack,
      attackLocation )

-- The printout at the start of the game
gameStartPrintout :: IO ()
gameStartPrintout = putStrLn "Captain! An enemy fleet has appeared within striking distance!"

gameOver :: IO ()
gameOver = putStrLn "GAME OVER"

enemyRetreating :: IO ()
enemyRetreating = do
  putStrLn "The enemy is retreating!"
  putStrLn "We're counting that as a victory."
  gameOver

-- Read the attack that the connection sends and update the game state accordingly
getAttack :: Socket -> GameState -> IO ()
getAttack s gs = do
  putStrLn "The enemy is attacking!" >> putStrLn "..."
  msg <- recv s 1024
  let msg1 = C.unpack msg
  checkAttackNotEmpty s gs msg1

checkAttackNotEmpty :: Socket -> GameState -> String -> IO ()
checkAttackNotEmpty _ _ "" = enemyRetreating
checkAttackNotEmpty s gs msg = do
  let mAttack = readMaybe msg :: Maybe Attack
  getAttackHelper s gs mAttack

getAttackHelper :: Socket -> GameState -> Maybe Attack -> IO ()
getAttackHelper _ _ Nothing =
  putStrLn "Error: Recieved invalid attack from the other player. Closing connection."
getAttackHelper s gs (Just attack) = do
  let gameResponse = makeResponseFromAttack attack gs
  let gs1 = updateOnAttack attack gs
  handleAttack s gs1 gameResponse

-- Print result of connection's attack and make decisions about game continuation
handleAttack :: Socket -> GameState -> Response -> IO ()
handleAttack _ gs (Response (Square x y) _ _ True) = do
  displayBoards gs
  putStrLn ("Hit at " ++ (attackLocation x y) ++ "! They sunk our last ship! We're going down!")
  putStrLn "You lost..."
  gameOver
handleAttack s gs response = do
  tellResponse response
  sendAll s (C.pack (show response))
  sendAttack s gs
  where tellResponse (Response (Square x y) _ True _) =
          putStrLn ("Hit at " ++ (attackLocation x y) ++ "! They sunk our ship!")
        tellResponse (Response (Square x y) True _ _) =
          putStrLn ("They hit our ship at " ++ (attackLocation x y ++) "!")
        tellResponse (Response (Square x y) False _ _) =
          putStrLn ("They missed! They only hit " ++ (attackLocation x y) ++ "! Phew!")

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
  putStrLn ""
  putStrLn "We cannot attack there, Captain! Pick another target!"
  putStrLn ""
  sendAttack s gs
sendAttackHelper s gs (Just attack)
  | notPrevAttack attack gs = do
    putStrLn "Firing!"
    sendAll s (C.pack (show attack))
    getResponse s gs
  | otherwise = do
    putStrLn ""
    putStrLn "We already fired on that location, Captain! Pick another target!"
    putStrLn ""
    sendAttack s gs

-- Read the connection's response to the sent attack
getResponse :: Socket -> GameState -> IO ()
getResponse s gs = do
  msg <- recv s 1024
  let msg1 = C.unpack msg
  checkResponseNotEmpty s gs msg1

checkResponseNotEmpty :: Socket -> GameState -> String -> IO ()
checkResponseNotEmpty _ _ "" = enemyRetreating
checkResponseNotEmpty s gs msg = do
  let mResponse = readMaybe msg :: Maybe Response
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
  gameOver
handleResponse s gs response = do
  tellResponse response
  getAttack s gs
  where tellResponse (Response  _ _ True _) = putStrLn "Hit! You sunk a ship!"
        tellResponse (Response  _ True _ _) = putStrLn "That's a hit!"
        tellResponse (Response _ False _ _) = putStrLn "That's a miss. We'll get 'em next time!"

displayBoards :: GameState -> IO ()
displayBoards (GameState bss bv) = putStrLn (showBoards bv)
