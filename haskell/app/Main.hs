module Main where

import Client
import Server

main :: IO ()
main = do
  putStrLn "Welcome to Battleship. Would you like to start a game or join one? [S/J]"
  msg <- getLine
  if msg == "S"
    then createServer
    else
      if msg == "J"
        then createClient
        else putStrLn "Incorrect option. Try again!"
