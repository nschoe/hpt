{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module Main (main) where


import           Control.Concurrent (ThreadId, forkIO)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (newTVar, readTVar)
import           Control.Exception (throwTo, AsyncException(ThreadKilled))
import           Data.Binary (encode, decode)
import qualified Data.ByteString.Char8 as B8 (putStrLn)
import qualified Data.ByteString.Lazy as L (fromStrict, toStrict, hGetContents, null)
import qualified Data.Map.Strict as Map (Map, insert, empty)
import           Data.Monoid ((<>))
import           Data.Time.Clock (getCurrentTime, UTCTime)
import           Dispatcher.Connection
import           Dispatcher.Tools
import           Network.Simple.TCP.TLS
import           Network.TLS.Extra (fileReadPrivateKey, fileReadCertificate)
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath ((</>))
import           System.IO (hSetBuffering, BufferMode(..), stdout, withFile, IOMode(..))
import           Text.Printf (printf)
import           Types
{- | Dispatcher executable for hpt.
   The dispatcher keeps a list of connected (alive) users along with their ip address.
   It also keeps a list of registered users with the hash of their passwords to ensure
   unicity of identities.
-}

{- | Run the dispatcher server :
   Initiate connected (alive) users list, load registered users list.
   Start a thread that saves registered users list to file every 5 minutes.
   Listens for incomming connections.
-}
main :: IO ()
main = getArgs >>= go

    where
      -- long options
      go ("--version":_) = putStrLn version
      go ("--help":_)    = putStrLn usage
      go ("--start":_)   = server
      -- short options
      go ("-v":_) = go ["--version"]
      go ("-h":_) = go ["--help"]
      go ("-s":_) = go ["--start"]
      go _        = putStrLn ("Wrong argument.\n" ++ usage)

{- | Load SSL certificate and private keys settings.
   Load registered users list.
   Fork a thread to automatically save registered users list to file.
   Start the server and listen for incoming connection.
-}
server :: IO ()
server = withSocketsDo $ do
  -- Construct Credential information
  putStr "Loading certificate..."
  certFile <- getCertificateFile
  certificate <- case certFile of
                   Left err ->
                       do
                         failed ("failed\n" ++ err ++ "\n")
                         exitFailure
                   Right certificateFile ->
                       do
                         fileReadCertificate certificateFile
  success "success\n"

  putStr "Loading private key..."
  keyFile <- getPrivateKeyFile
  privKey     <- case keyFile of
                   Left err -> 
                       do
                         failed ("failed\n" ++ err ++ "\n")
                         exitFailure
                   Right privateKeyFile ->
                       do
                         fileReadPrivateKey privateKeyFile
  success "success\n"

  putStr "Creating server credential..."
  let cred = Credential certificate privKey []
  success "success\n"

  -- Create server SSL settings
  putStr "Creating server SSL settings..."
  let servSettings = makeServerSettings cred Nothing
  success "success\n"

  -- Reading registered users database
  putStr "Reading registered users database..."
  regDb <- getRegisteredDb
  dbFile <- case regDb of
              Left err ->
                  do
                    failed ("failed\n" ++ err ++ "\n")
                    exitFailure
              Right file ->
                  return file
  -- TEMPORARY : using read and show but FIX with Binary
  raw <- withFile dbFile ReadMode $ \h ->
         do !contents <- L.hGetContents h
            return contents
            
  let reg = case L.null raw of
              True -> Map.empty
              False -> decode raw :: Map.Map UserName HashPassword
  success "success\n"

  -- Create internal state
  putStr "Initializing currently connected users list..."
  aliveUsers <- atomically $ newTVar (Map.empty)
  success "success\n"

  putStr "Creating empty registered users list..."
  registeredUsers <- atomically $ newTVar reg
  success "success\n"

  putStr "Creating internal state..."
  let state = DispatcherState {
                           dsAlive = aliveUsers
                         , dsRegistered = registeredUsers
                         }
  success "success\n"

  -- Start the server
  hSetBuffering stdout NoBuffering
  printf "Starting dispatcher on port %s...\n\n" port
  tId <- forkIO $ serve servSettings HostIPv6 port (handleClient state)

  -- Loop and parse dispatcher administrative commands
  loop state tId dbFile

  putStrLn "Server exiting..."
      where
        loop state tId dbFile = do
          putStr "Administrative dispatcher command : "
          cmd <- getLine
          putStr "\n"
          case cmd of
            "help" -> putStrLn ("Available commands : help, stop, list alive, list registered, save") >> loop state tId dbFile
            "stop" -> putStrLn "Dispatcher exiting..." >> throwTo tId ThreadKilled
            "list alive" -> do
                           clientsmap <- atomically $ readTVar (dsAlive state)
                           putStrLn ("Alive users : \n" ++ show clientsmap)
                           loop state tId dbFile
            "list registered" -> do
                           regmap <- atomically $ readTVar (dsRegistered state)
                           putStrLn ("Registered users : \n" ++ show regmap)
                           loop state tId dbFile
            "save" -> do
                     putStr "Saving..."
                     saveDb state dbFile
                     success "success\n"
                     loop state tId dbFile
            _ -> putStrLn ("Unknown command : " ++ cmd) >> loop state tId dbFile


-- | Deal with a client connecting
handleClient :: DispatcherState -> (Context, SockAddr) -> IO()
handleClient state (context, sockaddr) = do

  -- Displays a message on the server
  putStrLn ("Incoming connection : " ++ show sockaddr)
  putStrLn "Waiting for authentification..."

  -- Waits for a Born request from the client
  mbs <- recv context
  putStr "Authentification incoming..."
  case mbs of
    Nothing ->
        do
          failed "failed\n"
          putStrLn "Contact lost with client in authentification phase."
    Just bs | isBornRequest decodedReq -> do
             success "success\n"            
             now <- getCurrentTime
             (logMsg, answer) <- atomically $ checkAuth state decodedReq now
             send context answer
             case logMsg of
               Left failMsg     -> do 
                           failed failMsg
                           return ()
               Right successMsg -> do
                           success successMsg
                           loop decodedReq
                                   
            | otherwise -> do
             failed "failed\n"
             putStrLn "Wrong request type received from client in authentification phase."
                 where decodedReq = decode (L.fromStrict bs)
                       loop req@(Born username hashpassword) = do 
                             mbs <- recv context
                             case mbs of
                               Nothing -> do
                                           failed ("Lost contact with " ++ username ++ ".\n")
                                           atomically $ removeFromAlive state username
                               Just bs -> do
                                           B8.putStrLn ( "Received : " <> bs )
                                           loop req

