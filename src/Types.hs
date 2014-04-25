module Types (
               UserName
             , ContactList
             , HashPassword
             , IpAddr
             , Message
             , Contact
             , Status
             , DispatcherRequest
             , DispatcherAnswer
             , Command
             ) where

import qualified Data.ByteString.Lazy as L (ByteString)

-- | Every type used in hpt and hpt-dispatcher

type UserName     = String
type ContactList  = [Contact]
type HashPassword = L.ByteString
type IpAddr       = String
type Message      = L.ByteString

-- | Contact data type : very little information is stored
data Contact = Contact {
      contactUserName :: UserName
    , contactStatus   :: Status
    , contactIpAddr   :: IpAddr
} deriving (Show, Eq)

-- | Contact status
data Status =
      Available     -- ^ Contact is ready to talk
    | Busy          -- ^ Contact is likely not to answer
    | NotAvailable  -- ^ Offline, not connected
      deriving (Eq, Show)

-- | Requests clients send the dispatcher
data DispatcherRequest =
      Born UserName HashPassword  -- ^ identification request (registers if user name doesn't exist)
    | Alive ContactList           -- ^ tell dispatcher we're connected (alive) and request contacts status
    | Suicide                     -- ^ tell dispatcher we want to disconnect (die) so that he can close socket
      deriving (Eq, Show)

-- | Messages the dispatcher sends back to clients
data DispatcherAnswer =
      BornOK                   -- ^ notify client that his Born request was ok
    | BornKO (Maybe String)    -- ^ notify client that his Born request failed
    | ReportStatus ContactList -- ^ tell client the status of his contact list
    | Die                      -- ^ tell client its Suicide (logout) request is ok, can close socket
      deriving (Eq, Show)

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
