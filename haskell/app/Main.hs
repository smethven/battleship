module Main where

import Client
import Server

main :: IO ()
main = do
  putStrLn "Welcome to Battleship! Would you like to start a game or join one? [S/J]"
  msg <- getLine
  if startGame msg
    then createServer
    else
      if joinGame msg
        then createClient
        else putStrLn "Incorrect option. Try again!" >> main

startGame :: String -> Bool
startGame "S" = True
startGame "s" = True
startGame "Start" = True
startGame "start" = True
startGame _ = False

joinGame :: String -> Bool
joinGame "J" = True
joinGame "j" = True
joinGame "Join" = True
joinGame "join" = True
joinGame _ = False