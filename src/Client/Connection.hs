module Client.Connection (
                           port
                         , clientPort
                         , keepAlive
                         ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar)
import qualified Data.ByteString.Lazy as L (toStrict)
import Data.Binary (encode)
import Network.Simple.TCP.TLS (Context, send)
import Types (ClientState(..), Contact(..), DispatcherRequest(..))

-- | Port on which the dispatcher listens to for incoming connection
port :: String
port = "1089"

-- | Port on which the user's server part listens to for incoming connection
clientPort :: String
clientPort = "9801"

-- | Maintains a connection with the dispatcher by sending Alive requests
-- along with contacts status request
keepAlive :: Context -> ClientState -> IO ()
keepAlive context state = do
  -- get contact list
  contacts <- atomically $ readTVar (csContactList state)
  
  -- extract list of usernames
  let names = map contactUserName contacts

  -- Build Alive request and send it to the dispatcher
  let req = Alive names
  send context (L.toStrict $ encode req)
