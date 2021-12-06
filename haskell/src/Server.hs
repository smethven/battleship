-- {-# LANGUAGE OverloadedStrings #-}

module Server (createServer) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO (getLine)
import Battleship --(GameState, Attack, Response, showBoards, parseAttack, updateOnResponse)

{- server socket creation adapted from https://hackage.haskell.org/package/network-3.1.2.5/docs/Network-Socket.html -}

runServer :: Socket -> IO ()
runServer s = do
  putStrLn "Let the battle begin!"
  runGame s serverStartState

runGame :: Socket -> GameState -> IO ()
runGame s gs = do
  -- The server has the first move
  gs1 <- sendServerAttack s gs
  gs2 <- getClientResponse s gs1
  gs3 <- getClientAttack s gs2
  gs4 <- sendServerResponse s gs3
  runGame s gs4

-- Read attack that user passes in and send it to the client
sendServerAttack :: Socket -> GameState -> IO ()
sendServerAttack s gs = do
  displayBoards gs
  putStrLn "Where do we fire, Captain? (Type a letter and a number, separated by a space)"
  msg <- getLine
  mAttack <- parseAttack msg
  sendServerAttackHelper s gs mAttack

sendServerAttackHelper :: Socket -> GameState -> Maybe Attack -> IO ()
sendServerAttackHelper s gs Nothing = sendServerAttack s gs
sendServerAttackHelper s gs (Just attack) =
  if notPrevAttack attack gs then sendAll s (C.pack (show attack)) else sendServerAttack s gs

-- Read the client's response to the server attack
getClientResponse :: Socket -> GameState -> IO ()
getClientResponse s gs = do
  msg <- recv s 1024
  response <- read msg :: Response
  gs1 <- updateOnResponse response gs
  tellResponse response
  displayBoards gs1
  --   TODO: Parse the msg into a hit or miss and update game state accordingly
  C.putStr msg

-- Read the attack that the client sends and update the game state accordingly
getClientAttack :: Socket -> GameState -> IO ()
getClientAttack s = do
  msg <- recv s 1024
  --   TODO: Parse the msg into an coordinate and update game state accordingly
  C.putStr msg

sendServerResponse :: Socket -> GameState -> IO ()
sendServerResponse s = do
  msg <- getLine
  --   TODO: Parse the message into a hit or miss and forward it to the client
  sendAll s (C.pack (msg ++ "\n"))

tellResponse :: Response -> IO ()
tellResponse _ = undefined -- TODO

displayBoards :: GameState -> IO ()
displayBoards (GameState bss bv) = showBorads bv


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
        listen sock 1024
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