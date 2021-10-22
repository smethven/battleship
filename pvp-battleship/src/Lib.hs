module Lib ( createSocket ) where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import qualified Control.Exception as E

{- server socket creation adapted from https://hackage.haskell.org/package/network-3.1.2.5/docs/Network-Socket.html#g:1 -}

resolve :: IO AddrInfo
resolve = do {
    ; let hints = Just $ defaultHints { addrFlags = [AI_PASSIVE, AI_NUMERICSERV], addrSocketType = Stream }
    ; head <$> getAddrInfo hints Nothing (Just "3000")
}

open :: AddrInfo -> IO Socket
open addr = E.bracketOnError (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)) close (\sock -> do {
    ; setSocketOption sock ReuseAddr 1
    ; withFdSocket sock setCloseOnExecIfNeeded
    ; bind sock $ addrAddress addr
    ; putStrLn "Bound to port"
    ; listen sock 1
    ; putStrLn "Listening..."
    ; return sock
})

acceptConn :: Socket -> IO ()
acceptConn sock = E.bracketOnError (accept sock) (close . fst) respond
    where respond (connection, _peer) = do {
        ; putStrLn "Connected to client!"
        -- ; msg <- recv connection 1024
        -- ; sendAll connection msg
        ; sendAll connection (C.pack "Hello World")
    }

createSocket :: IO ()
createSocket = do {
    ; putStrLn "!!! To test proof of concept: !!!"
    ; putStrLn "Run on the CS department Linux servers, and on the same server in a different terminal run 'nc localhost 3000'."
    ; putStrLn "Note: Running nc on the same server is only for the convenience of using 'localhost' instead of finding the proper IP address. It still demonstrates a proper network/socket connection."
    ; putStrLn ""
    ; addr <- resolve
    ; putStrLn "Resolved AddrInfo struct"
    ; E.bracket (open addr) close acceptConn
}