{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Exception                        ( finally )
import           Data.Text.Encoding                       ( encodeUtf8 )
import qualified Data.Text                     as T
import           Control.Concurrent
import           Control.Monad                            ( forM_
                                                          , forever
                                                          )
-- import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString               as B
import           Data.Text                                ( Text )
import qualified Network.WebSockets            as WS

-- | Simple `(name, connection)` client connection representation.
type Client = (Text, WS.Connection)


-- | Server will track the currently connected clients. Clients are unique and
-- identified by their `name`.
type ServerState = [Client]

-- | Initialise the server state
newServerState :: ServerState
newServerState = []

clientExists :: Client -> ServerState -> Bool
clientExists (name, _) = any ((== name) . fst)


addClient, removeClient :: Client -> ServerState -> ServerState
addClient = (:)
removeClient client = filter ((/= fst client) . fst)

broadcast :: B.ByteString -> ServerState -> IO ()
broadcast message clients =
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message


application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg     <- WS.receiveData conn
  clients <- readMVar state
  case msg of
    _
      | not (prefix `T.isPrefixOf` msg) -> WS.sendTextData
        conn
        ("bad join" :: Text)
      | clientExists client clients -> WS.sendTextData
        conn
        ("already joined" :: Text)
      | otherwise -> flip finally disconnect $ do
        modifyMVar_ state $ \s -> do
          let s' = addClient client s
          WS.sendTextData conn ("welcome" :: Text)
          broadcast ("joined:" `mappend` encodeUtf8 (fst client)) s'
          return s'
        talk client state
     where
       -- Prefix for channel join messages
      prefix     = "join:"
      -- Currently joining client
      client     = (T.drop (T.length prefix) msg, conn)
      -- Remove the captured client and broadcast that leaving to everyone
      -- remaining
      disconnect = do
        s <- modifyMVar state
          $ \s -> let s' = removeClient client s in return (s', s')
        broadcast ("left:" `mappend` encodeUtf8 (fst client)) s


talk :: Client -> MVar ServerState -> IO ()
talk (_, conn) state = forever $ do
  msg <- WS.receiveData conn
  readMVar state >>= broadcast msg

