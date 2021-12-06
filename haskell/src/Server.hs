-- {-# LANGUAGE OverloadedStrings #-}

module Server (createServer) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Gamestate ( serverStartState )
import Gameplay ( sendAttack, gameStartPrintout )

{- server socket creation adapted from https://hackage.haskell.org/package/network-3.1.2.5/docs/Network-Socket.html -}

runServer :: Socket -> IO ()
runServer s = do
  gameStartPrintout
  sendAttack s serverStartState

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
        listen sock 1024
        return sock
    )

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

createSocket :: IO ()
createSocket = do
  addr <- resolve
  E.bracket (open addr) close acceptConn

createServer :: IO ()
createServer = createSocket