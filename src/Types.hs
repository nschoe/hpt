{-# LANGUAGE DeriveGeneric #-}

module Types (
               UserName
             , ContactList
             , HashPassword
             , IpAddr
             , Message(..)
             , Contact(..)
             , Status(..)
             , DispatcherRequest(..)
             , DispatcherAnswer(..)
             , Command(..)
             , DispatcherEntry(..)
             , DispatcherState(..)
             , ClientState(..)
             , Conversation(..)
             ) where

import           Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)
import qualified Crypto.Hash.SHA512 as SHA512
import           Data.Binary
import qualified Data.ByteString as B (ByteString)
import  Data.Dequeue (BankersDequeue)
import qualified Data.Map.Strict as Map (Map)
import           Data.Time.Clock (UTCTime)
import           Network.Simple.TCP.TLS (Context)
import Network.Simple.TCP (Socket)
import GHC.Generics (Generic)
-- | Every type used in hpt and hpt-dispatcher

type UserName     = String
type ContactList  = [Contact]
type HashPassword = B.ByteString
type IpAddr       = String


-- | Contact data type : very little information is stored
data Contact = Contact {
      contactUserName :: UserName
    , contactStatus   :: Status
    , contactIpAddr   :: IpAddr
} deriving (Show, Eq, Generic)

instance Binary Contact

-- | Contact status
data Status =
      Available     -- ^ Contact is ready to talk
    | Busy          -- ^ Contact is likely not to answer
    | NotAvailable  -- ^ Offline, not connected
      deriving (Eq, Show, Generic)

instance Binary Status

-- | Different types of messages
data Message =
      Notice String               -- ^ Simple notice (e.g. a contact logs int/out, etc)
    | PingMe                      -- ^ A ping message to wake up a not-responding contact
    | Message String              -- ^ plain text message from a contact
      deriving (Show, Eq)
-- | Requests clients send the dispatcher
data DispatcherRequest =
      Born UserName HashPassword  -- ^ identification request (registers if user name doesn't exist)
    | Alive [UserName]            -- ^ tell dispatcher we're connected (alive) and request contacts status
    | Suicide                     -- ^ tell dispatcher we want to disconnect (die) so that he can close socket
      deriving (Eq, Show, Generic)

instance Binary DispatcherRequest

-- | Messages the dispatcher sends back to clients
data DispatcherAnswer =
      BornOK (Maybe String)    -- ^ notify client that his Born request was ok
    | BornKO (Maybe String)    -- ^ notify client that his Born request failed
    | ReportStatus ContactList -- ^ tell client the status of his contact list
    | Die                      -- ^ tell client its Suicide (logout) request is ok, can close socket
    | Error (Maybe String)     -- ^ report an error to the client
      deriving (Eq, Show, Generic)

instance Binary DispatcherAnswer

-- | Commands interpreted from user input
data Command =
     Quit                               -- ^ quit hpt
   | ListContact                        -- ^ get contact list status
   | ChangeStatus Status                -- ^ set new status
   | SendMessage UserName Message       -- ^ send a message to a user (must have started conversation before)
   | StartConversation UserName         -- ^ starts a conversation with a user (allows sending message)
   | StopConversation UserName          -- ^ stops a conversation with a user (close sockets)
   | Ping UserName                      -- ^ send a user a ping request to wake him when not answering
   | AddContact UserName                -- ^ add a contact to one's contact list
   | DeleteContact UserName             -- ^ remove a contact from one's contact list
     deriving (Eq, Show)

{- | Internal state maintained by the dispatcher :
   A map from user name to hash of password to keep track of all registered users.
   A map from user name to a DispatcherEntry containing a user's ip address, its last timestamp and its current status
-}
data DispatcherState = DispatcherState {
      dsAlive      :: TVar (Map.Map UserName DispatcherEntry)
    , dsRegistered :: TVar (Map.Map UserName HashPassword)
}

--instance Binary (Map.Map UserName HashPassword)

-- | Type to represent the (very little) information stored by the dispatcher
data DispatcherEntry = DispatcherEntry {
      deIpAddr :: IpAddr                -- ^ user's ip address
    , deTime   :: UTCTime               -- ^ timestamp of last activity (Alive request)
    , deStatus :: Status                -- ^ user's current status
    }
                       deriving (Show, Eq)
{- | Internal state maintained by the client :
   A map from user name to context (opened conversations with contacts)
   The contact list
   A TChan to receive all messages from every conversations
-}
data ClientState = ClientState {
      csConversations :: TVar (Map.Map UserName Conversation)  -- ^ list of started conversations
    , csContactList   :: TVar (ContactList)                    -- ^ the user's contact list
    , csCommands      :: TChan Command                         -- ^ commands to send to the dispatcher
    , csCurrentChat   :: Maybe UserName                        -- ^ current active conversation
    , csContactListDb :: FilePath                              -- ^ file to save the contact list to
    }
                   deriving (Eq)
-- | Describe am opened conversation with a contact
data Conversation = Conversation {
      convContext :: Socket                    -- ^ 'socket' to send and receive message. TEMPORARY : replace with SSL/TLS Context
    , convHistory :: BankersDequeue Message    -- ^ list of messages
    , convNbNew   :: Int                       -- ^ nb of new messages since last read
}
