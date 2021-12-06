-- {-# LANGUAGE OverloadedStrings #-}

module Server (createServer) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO (getLine)

{- server socket creation adapted from https://hackage.haskell.org/package/network-3.1.2.5/docs/Network-Socket.html -}

runServer :: Socket -> IO ()
runServer s = do
  sendAll s (C.pack "Welcome to Battleship!\n")
  C.putStrLn (C.pack "Welcome to Battleship!\n")
  --   Write any other game init stuff here
  runGame s

runGame :: Socket -> IO ()
runGame s = do
  -- Assuming that the server goes first
  getServerInput s
  getClientInput s
  runGame s

getServerInput :: Socket -> IO ()
getServerInput s = do
  msg <- getLine
  sendAll s (C.pack (msg ++ "\n"))

getClientInput :: Socket -> IO ()
getClientInput s = do
  msg <- recv s 1024
  --   putStr "Received: "
  C.putStr msg

resolve :: IO AddrInfo
resolve = do
  let hints = Just $ defaultHints {addrFlags = [AI_PASSIVE, AI_NUMERICSERV], addrSocketType = Stream}
  head <$> getAddrInfo hints Nothing (Just "3000")

openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

open :: AddrInfo -> IO Socket
open addr =
  E.bracketOnError
    (openSocket addr)
    close
    ( \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        -- putStrLn "Bound to port"
        listen sock 1
        -- putStrLn "Listening..."
        return sock
    )

acceptConn :: Socket -> IO ()
acceptConn sock =
  E.bracketOnError
    (accept sock)
    (close . fst)
    ( \(connection, _peer) -> do
        putStrLn "Connected to client!"
        runServer connection
    )

createSocket :: IO ()
createSocket = do
  -- putStrLn "!!! To test proof of concept: !!!"
  -- putStrLn "Run on the CS department Linux servers or a Mac, and on the same server/Mac in a different terminal run 'nc localhost 3000'."
  -- putStrLn "Note: Running nc on the same server/Mac is only for the convenience of using 'localhost' instead of finding the proper IP address. It still demonstrates a proper network/socket connection. While the the Network.Socket package should work on Windows, nc seems to struggle to connect to Windows sockets."
  -- putStrLn ""
  addr <- resolve
  -- putStrLn "Resolved AddrInfo struct"
  E.bracket (open addr) close acceptConn

createServer :: IO ()
createServer = createSocket