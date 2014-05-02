{-# LANGUAGE OverloadedStrings #-}

module Dispatcher.Connection (
                               port
                             , getPrivateKeyFile
                             , getCertificateFile
                             , getRegisteredDb
                             , checkAuth
                             , removeFromAlive
                             ) where
import           Control.Concurrent.STM
import Network.Socket.Internal (SockAddr(..))
import           Control.Concurrent.STM.TVar
import           Data.Binary (encode, decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L (toStrict)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Time.Clock (UTCTime)
import           System.Directory (getAppUserDataDirectory, getPermissions, readable, doesDirectoryExist, doesFileExist)
import           System.Environment (getProgName)
import           System.FilePath ((</>))
import           Types

-- | Port on which the dispatcher listens to for incoming connection
port :: String
port = "1089"

-- | Path to the file containing the private key
getPrivateKeyFile :: IO (Either String FilePath)
getPrivateKeyFile = do
  -- get user's app data directory
  dataDir <- getProgName >>= getAppUserDataDirectory

  -- check if it exists
  dirExists <- doesDirectoryExist dataDir
  if not dirExists then
      return (Left "User app data doesn't exist.")
  else
      do
        -- Check if private key exists
        let privateKey = dataDir </> "privkey.pem"
        keyExists <- doesFileExist privateKey
        if not keyExists then
            return (Left "Private key not found")
        else
            do
              -- Check if readable
              keyReadable <- readable `fmap` getPermissions privateKey
              if not keyReadable then
                  return (Left "Private key file not readable : permissions not granted")
              else
                  return (Right privateKey)

--privateKeyFile = "/home/nschoe/workspace/haskell/hpt/src/Dispatcher/ssl/privkey.pem"

-- | Path to the file containing the x509 certificate
getCertificateFile :: IO (Either String FilePath)
getCertificateFile = do
  -- get user's app data directory
  dataDir <- getProgName >>= getAppUserDataDirectory

  -- check if it exists
  dirExists <- doesDirectoryExist dataDir
  if not dirExists then
      return (Left "User app data doesn't exist.")
  else
      do
        -- Check if certificate exists
        let certificate =  dataDir </> "cacert.pem"
        certificateExists <- doesFileExist certificate
        if not certificateExists then
            return (Left "Certificate not found")
        else
            do
              -- Check if readable
              certificateReadable <- readable `fmap` getPermissions certificate
              if not certificateReadable then
                  return (Left "Certificate file not readable : permissions not granted")
              else
                  return (Right certificate)

-- | Path to the file containing the list of registered users
getRegisteredDb :: IO (Either String FilePath)
getRegisteredDb = do
  -- get user's app data directory
  dataDir <- getProgName >>= getAppUserDataDirectory

  -- check if it exists
  dirExists <- doesDirectoryExist dataDir
  if not dirExists then
      return (Left "User app data doesn't exist.")
  else
      do
        -- Check if db exists
        let dbFile = dataDir </> "registered.db"
        dbExists <- doesFileExist dbFile
        if not dbExists then
            do
              -- Create empty file
              writeFile dbFile ""
              return (Right dbFile)
        else
            return (Right dbFile)

--certificateFile = "/home/nschoe/workspace/haskell/hpt/src/Dispatcher/ssl/cacert.pem"

{- | Run the authentification scheme : make proper verifications and update state
   Authentification scheme :
   1. check the user is not already connected (fail in this case, but log out in further versions
   2. check if the user is registered
      2.a if yes, check if the password matches
      2.b if no, then register the user with the provided password
   The returned ByteString is the message to send back the client
   The Either String String is for the dispatcher to determine if the auth failed (Left) or not (Right)
-}
checkAuth :: DispatcherState -> DispatcherRequest -> UTCTime -> SockAddr -> STM (Either String String, B.ByteString)
checkAuth state (Born username hashpassword) currentTime sockaddr = do
  -- Get the two maps
  clientsmap <- readTVar (dsAlive state)
  regmap     <- readTVar (dsRegistered state)

  -- 1. check if connected
  if Map.member username clientsmap then
      -- Yes (connected) : fail because user is already connected (log out in future implementation)
      return (
               Left $ "Error : " ++ username ++ " is already connected.\nAbord.\n"
             , L.toStrict $ encode (BornKO (Just ("Username \"" <> username <> "\" is already connected.")))
             )
  else
      -- No (not connected) : check if registered
      if Map.member username regmap then do
          -- Yes (registered) : check if passwords match
          -- can use hard pattern-matching here because we made sure it would work with Map.member
          let Just storedPwdHash = Map.lookup username regmap
          if hashpassword == storedPwdHash then do
              -- Yes (passwords match) : log in
--              let (SockAddrInet6 _ _ ip _) = sockaddr
              let (SockAddrInet _ ip) = sockaddr
              let newEntry = DispatcherEntry (show ip) currentTime Available
              writeTVar (dsAlive state) (Map.insert username newEntry clientsmap)
              return (
                       Right $ username ++ " successfully logged in.\n"
                     , L.toStrict $ encode (BornOK (Just "Authentification successful"))
                     )
          else
              -- No (passwords don't match) : fail and don't log in
              return (
                       Left $ "Password for user \"" ++ username ++ "\" did not match"
                     , L.toStrict $ encode (BornKO (Just "Password Mismatch"))
                     )
      else do
        -- No (not registered) : register the user and log him in
        writeTVar (dsRegistered state) (Map.insert username hashpassword regmap)
        let newEntry = DispatcherEntry "ip addr" currentTime Available
        writeTVar (dsAlive state) (Map.insert username newEntry clientsmap)
        return (
                 Right $ username ++ " registered.\n"
               , L.toStrict $ encode (BornOK (Just $ "You are now registered as " ++ username))
               )
        
-- Can't happen (check with isBornRequest before
checkAuth _ _ _ _ = error "The authentification check is done only upon connection (i.e. on Born request)"

-- | Remove a username from the alive clients map.
-- Called upon logout requests of loss of connection
removeFromAlive :: DispatcherState -> UserName -> STM ()
removeFromAlive state username = do
  clientsmap <- readTVar (dsAlive state)
  writeTVar (dsAlive state) (Map.delete username clientsmap)
  
