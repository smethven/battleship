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

checkHostAddr :: String -> String
checkHostAddr "localhost" = "127.0.0.1"
checkHostAddr string = string

resolve :: IO AddrInfo
resolve = do
  let hints = defaultHints {addrSocketType = Stream}
  putStrLn "What host would you like to connect to?"
  putStrLn "Please type 'localhost' or an IPv4 address in dot notation."
  host <- getLine
  let hostAddr = checkHostAddr host
  head <$> getAddrInfo (Just hints) (Just hostAddr) (Just "3000")

openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

attemptConnection :: AddrInfo -> Socket -> IO Socket
attemptConnection sock =
  E.onException
    (connect sock $ addrAddress addr >> return sock)
    (putStrLn "Unable to connect to the given address." >> close sock)

open :: AddrInfo -> IO Socket
open addr =
  E.bracketOnError
    (openSocket addr)
    close
    (attemptConnection addr)

createSocket :: IO ()
createSocket = do
  addr <- resolve
  E.bracket (open addr) close runClient

createClient :: IO ()
createClient = createSocket