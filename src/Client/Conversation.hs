module Conversation (
                      startConversation
                    , historyMaxLength
                    ) where

import Data.Maybe (isNothing, fromJust)
import qualified Data.ByteString.Char8 as B8 (unpack)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import qualified Data.Map.Strict as Map (lookup, insert)
import Types (ClientState(..))
import Network.Simple.TCP (withSocketsDo, connect)
import Tools (contactLookup)

-- | Number of messages to store in the history
historyMaxLength :: Int
historyMaxLength = 100

{- | Start a new conversation with a contact :
   - Establishes a connection with the user
   - Updates the state with the new Conversation
   - fork a thread to listen to incoming messages and put them in the Conversation's History
-}
startConversation :: ClientState -> UserName -> IO ()
startConversation state username = do
  -- Establish connection (to be replaced with SSL/TLS version !)
  do contacts <- atomically $ readTVar (csContactList state)
     let mContact = contactLookup username contacts
     when (not (isNothing mContact)) $
          do let contact = fromJust mContact
             connect (contactIpAddr contact) clientPort (receiveThread state username)

-- | Initiate a conversation and handle the exchange of messages
receiveThread :: ClientState -> UserName -> (Socket, SockAddr) -> IO ()
receiveThread state username (socket, _) = do
  -- Add the new Conversation to the state
  now <- getCurrentTime
  let conversation = Conversation {
                                 convContext = Socket
                               , convHistory = Q.fromList [Notice ("Conversation started on " ++ now)]
                               , convNbNew   = 1
                               }
  atomically $ do
    conversations <- readTVar csConversations state
    writeTVar conversations (Map.insert username conversation conversations)

  -- loop listening for incoming messages
  loop state username socket
      where
        loop state username socket = do
          mbs <- recv socket 1024
          case mbs of
            Nothing -> do putStrLn ("Connection closed with " ++ username)
            Just bs ->
                do conversations <- atomically $ readTVar (csConversations state)
                   let (Just conversation) = Map.lookup username conversations
                   let newHistory = pushFront (convHistory conversation) (B8.unpack bs) -- don't parse, only Message for now (TEMPORARY)
                   let newConversation = conversation { convHistory = newHistory, convNbNew = convNbNew + 1 }
                   atomically $ writeTVar (csConversation state) newConversation
                   loop state username socket
