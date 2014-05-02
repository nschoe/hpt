{-# LANGUAGE BangPatterns #-}

module Client.Connection (
                           port
                         , clientPort
                         , keepAlive
                         , getContactListDb
                         , saveContactList
                         ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar)
import qualified Data.ByteString.Lazy as L (toStrict, hPut)
import Data.Binary (encode)
import Network.Simple.TCP.TLS (Context, send)
import Types (ClientState(..), Contact(..), DispatcherRequest(..))
import           System.Directory (getAppUserDataDirectory, getPermissions, readable, doesDirectoryExist, doesFileExist, createDirectoryIfMissing)
import           System.Environment (getProgName)
import System.FilePath ((</>))
import Types (ContactList, UserName)
import System.IO (withFile, IOMode(..))

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

-- | Path to the file containing the saved contact list
getContactListDb :: UserName -> IO (Either String FilePath)
getContactListDb username  = do
  -- get user's app data directory
  dataDir <- getProgName >>= getAppUserDataDirectory

  -- check if it exists
  dirExists <- doesDirectoryExist dataDir
  if not dirExists then
      return (Left "User app data doesn't exist.")
  else
      do
        -- create user directory if it doesn't exist
        let userDir = dataDir </> username
        createDirectoryIfMissing False userDir

        -- Check if db exists
        let dbFile = userDir </> (username ++ ".contactList.db")
        dbExists <- doesFileExist dbFile
        if not dbExists then
            do
              -- Create empty file
              writeFile dbFile ""
              return (Right dbFile)
        else
            return (Right dbFile)

-- | Write the contact list to file
saveContactList :: ClientState -> ContactList -> IO ()
saveContactList state contacts = do
  dbFile <- atomically $ readTVar (csContactListDb state)
  let !encoded = encode contacts
  withFile dbFile WriteMode (flip L.hPut encoded)
