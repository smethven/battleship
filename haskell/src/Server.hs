-- {-# LANGUAGE OverloadedStrings #-}

module Server (createServer) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Gameplay (gameStartPrintout, sendAttack)
import Gamestate (serverStartState)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

{- server socket creation adapted from https://hackage.haskell.org/package/network-3.1.2.5/docs/Network-Socket.html -}

-- Start a session of Battleship
runServer :: Socket -> IO ()
runServer s = do
  gameStartPrintout
  sendAttack s serverStartState

-- Set up info for a TCP(stream) socket that will accept connections on port 3000.
resolve :: IO AddrInfo
resolve = do
  let hints = Just $ defaultHints {addrFlags = [AI_PASSIVE, AI_NUMERICSERV], addrSocketType = Stream}
  head <$> getAddrInfo hints Nothing (Just "3000")

-- Create a socket based on the address
openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

-- We open the socket based on the address. We set te ability to reuse the port number so that on future runs of
-- the game, we don't get an error saying the port is reserved. We then grab a file descriptor for the socket. We bind
-- the socket to the addresses and listen for incoming connections. We only allow one queued client.
open :: AddrInfo -> IO Socket
open addr =
  E.bracketOnError
    (openSocket addr)
    close
    ( \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1
        return sock
    )

-- We wait for an incoming client connection. Once we get one, we accept it and redirect to runServer
-- which handles the logic used to run the game. Once all the remaining parts have completed execution,
-- we close the socket.
acceptConn :: Socket -> IO ()
acceptConn sock = do
  putStrLn "Waiting for a player to connect..."
  putStrLn ""
  E.bracketOnError
    (accept sock)
    (close . fst)
    ( \(connection, _peer) -> do
        runServer connection
    )

-- We first determine the IP/Port address we are using to identify the socket. With the address
-- we then redirect to acceptConn which waits for a Client connection. Once all the remaining
-- parts have completed execution, we then teardown the connection.
createSocket :: IO ()
createSocket = do
  addr <- resolve
  E.bracket (open addr) close acceptConn

-- Redirect to creating a socket used to maintain the connection
createServer :: IO ()
createServer = createSocket