-- {-# LANGUAGE OverloadedStrings #-}

module Client (createClient) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO (getLine)

{- client socket creation adapted from https://hackage.haskell.org/package/network-3.1.2.5/docs/Network-Socket.html -}

runClient :: Socket -> IO ()
runClient s = do
  C.putStrLn (C.pack "Welcome to Battleship!\n")
  --   TODO: Write any other game init stuff here
  runGame s

runGame :: Socket -> IO ()
runGame s = do
  -- Assuming that the server goes first
  getServerAttack s
  sendClientResponse s
  sendClientAttack s
  getServerResponse s
  runGame s

-- Read attack that user passes in and send it to the client
sendClientAttack :: Socket -> IO ()
sendClientAttack s = do
  msg <- getLine
  --   TODO: Parse the msg into an coordinate forward it to the server
  sendAll s (C.pack (msg ++ "\n"))

sendClientResponse :: Socket -> IO ()
sendClientResponse s = do
  msg <- getLine
  --   TODO: Parse the message into a hit or miss and forward it to the server
  sendAll s (C.pack (msg ++ "\n"))

-- Read the attack that the server sends and update the game state accordingly
getServerAttack :: Socket -> IO ()
getServerAttack s = do
  msg <- recv s 1024
  --   TODO: Parse the msg into an coordinate and update game state accordingly
  C.putStr msg

-- Read the server's response to the client attack
getServerResponse :: Socket -> IO ()
getServerResponse s = do
  msg <- recv s 1024
  --   TODO: Parse the msg into a hit or miss and update game state accordingly
  C.putStr msg

resolve :: IO AddrInfo
resolve = do
  let hints = defaultHints {addrSocketType = Stream}
  --   TODO: Change 127.0.0.1 to a host
  head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "3000")

openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

open :: AddrInfo -> IO Socket
open addr =
  E.bracketOnError
    (openSocket addr)
    close
    ( \sock -> do
        connect sock $ addrAddress addr
        return sock
    )

createSocket :: IO ()
createSocket = do
  -- putStrLn "!!! To test proof of concept: !!!"
  -- putStrLn "Run on the CS department Linux servers or a Mac, and on the same server/Mac in a different terminal run 'nc localhost 3000'."
  -- putStrLn "Note: Running nc on the same server/Mac is only for the convenience of using 'localhost' instead of finding the proper IP address. It still demonstrates a proper network/socket connection. While the the Network.Socket package should work on Windows, nc seems to struggle to connect to Windows sockets."
  -- putStrLn ""
  addr <- resolve
  -- putStrLn "Resolved AddrInfo struct"
  E.bracket (open addr) close runClient

createClient :: IO ()
createClient = createSocket