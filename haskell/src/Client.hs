-- {-# LANGUAGE OverloadedStrings #-}

module Client (createClient) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Gamestate ( clientStartState )
import Gameplay ( getAttack, gameStartPrintout )

{- client socket creation adapted from https://hackage.haskell.org/package/network-3.1.2.5/docs/Network-Socket.html -}

runClient :: Socket -> IO ()
runClient s = do
  gameStartPrintout
  getAttack s clientStartState

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
  addr <- resolve
  E.bracket (open addr) close runClient

createClient :: IO ()
createClient = createSocket