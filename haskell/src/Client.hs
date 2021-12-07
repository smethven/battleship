-- {-# LANGUAGE OverloadedStrings #-}

module Client (createClient) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Gameplay (gameStartPrintout, getAttack)
import Gamestate (clientStartState)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

{- client socket creation adapted from https://hackage.haskell.org/package/network-3.1.2.5/docs/Network-Socket.html -}

-- Start a session of Battleship
runClient :: Socket -> IO ()
runClient s = do
  gameStartPrintout
  getAttack s clientStartState

-- Resolve localhost to IP of 127.0.0.1
checkHostAddr :: String -> String
checkHostAddr "localhost" = "127.0.0.1"
checkHostAddr string = string

-- Set up info for a TCP(stream) socket and prompt the user for a IP that it wants to target.
resolve :: IO AddrInfo
resolve = do
  let hints = defaultHints {addrSocketType = Stream}
  putStrLn "What host would you like to connect to?"
  putStrLn "Please type 'localhost' or an IPv4 address in dot notation."
  host <- getLine
  let hostAddr = checkHostAddr host
  head <$> getAddrInfo (Just hints) (Just hostAddr) (Just "3000")

-- Create a socket based on the address
openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

-- Attempt to start a connection to the address on the socket. If a connection isn't able to be
-- established, prompt an error.
attemptConnection :: AddrInfo -> Socket -> IO Socket
attemptConnection addr sock =
  E.onException
    ((connect sock $ addrAddress addr) >> return sock)
    (putStrLn "Unable to connect to the given address.")

-- We open the socket based on the address and attempt to iniate a connnection with a Server. Once
-- all the remaining parts have completed execution, we close the socket.
open :: AddrInfo -> IO Socket
open addr =
  E.bracketOnError
    (openSocket addr)
    close
    (attemptConnection addr)

-- We first determine the IP/Port address we are using to identify the socket. With the address
-- we then redirect to runClient which handles the logic used to run the game. Once all the remaining
-- parts have completed execution, we then teardown the connection.
createSocket :: IO ()
createSocket = do
  addr <- resolve
  E.bracket (open addr) close runClient

-- Redirect to creating a socket used to maintain the connection
createClient :: IO ()
createClient = createSocket