{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module Main (main) where

import           Client.Connection
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar, MVar)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TChan (newTChan, readTChan)
import           Control.Concurrent.STM.TVar (newTVar, readTVar, writeTVar)
import Control.Monad (when, forever)
import           Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8 (putStrLn, getLine, pack, dropWhile, words, getLine)
import qualified Data.ByteString.Lazy as L (toStrict, fromStrict, null, hGetContents)
import           Data.CertificateStore
import           Data.Char (isSpace)
import qualified Data.Map as Map (empty)
import           Data.Maybe (fromMaybe, fromJust, isNothing)
import           Data.Monoid ((<>))
import           Network.Simple.TCP.TLS
import           Network.TLS
import           Network.TLS.Extra (fileReadPrivateKey, fileReadCertificate)
import           System.Certificate.X509.Unix
import           System.Console.Haskeline (runInputT, getPassword, defaultSettings)
import           System.Environment (getArgs)
import           System.IO (hSetBuffering, BufferMode(..), IOMode(..), stdout, withFile)
import           Tools
import           Types
import           System.Exit (exitFailure)

{- | hpt main function.
   Start the client and the server parts of hpt.
-}
main :: IO ()
main = getArgs >>= go
    where
      -- long options
      go ("--version":_) = putStrLn version
      go ("--help":_)    = putStrLn usage
      go ("--start":hostname:_) = hpt hostname
      -- short options
      go ("-v":_) = go ["--version"]
      go ("-h":_) = go ["--help"]
      go ("-s":hostname:_) = go ["--start", hostname]
      go _        = putStrLn ("Wrong argument.\n" ++ usage)

-- | Start client and server parts
hpt :: String -> IO ()
hpt hostname = withSocketsDo $ do

        -- Construct credential information
        putStr "Loading certificate..."
        certificate <- fileReadCertificate "/home/nschoe/.hpt-dispatcher/cacert.pem"
        success "success\n"

        -- Get certificate store
        putStr "Getting client's system certificate store..."
        cStore <- getSystemCertificateStore
        success "success\n"

        -- TEMPORARY
        -- Create artificial certificate store to recognize dispatcher's certificate
        let certificateStore = makeCertificateStore [certificate]
               
        -- Create client SSL settings
        putStr "Creating client SSL settings..."
        let clientSettings = makeClientSettings [] (Just hostname) (certificateStore <> cStore)
        success "success.\n"

        -- Create internal state
        putStr "Initializing internal state..."
        conversations <- atomically $ newTVar (Map.empty)
        contactList   <- atomically $ newTVar []
        chanCommands  <- atomically $ newTChan
        currentChat   <- atomically $ newTVar Nothing
        contactListDb <- atomically $ newTVar ""

        let state = ClientState {
                                 csConversations = conversations
                               , csContactList   = contactList
                               , csCommands      = chanCommands
                               , csCurrentChat   = currentChat
                               , csContactListDb = contactListDb
                               }
        success "success\n"

        -- Connecting to the dispatcher
        hSetBuffering stdout NoBuffering     -- for the output to appear in the correct order
        authSuccess <- newEmptyMVar          -- create sync mvar : will be put to True when 'connect' successfully authenticated

        putStr "Connecting to dispatcher..."
        talkDispatcher_tId <- forkIO $ connect clientSettings hostname port (talkDispatcher state authSuccess)

        isAuthSuccessful <- takeMVar authSuccess

        -- When we successfully reached the Dispatcher and logged in, continue
        when isAuthSuccessful $ loop state

        -- Exit when auth was not successfull
        putStrLn "Authentification was not successful"
                     
        putStrLn "\nClient exiting.\n"
            where
              loop state = do
                -- ask for a command
                putStr "Input command (try '/help') : "
                cmd <- (words . dropWhile isSpace) `fmap` getLine -- just remove leading spaces

                -- parse command
                case cmd of
                  ("/help":_) ->
                      do putStrLn ("Must display help")
                  ("/list":_) ->
                      do contactList <- atomically $ readTVar (csContactList state)
                         let output1 = "\nContact list (" ++ show (length contactList) ++ " contacts) : [unread messages]\n"
                         let output2 = output1 ++ (replicate (length output1 - 2) '=') ++ "\n"
                         let list = map ((++ " [not implemented]") . contactUserName) contactList
                         let output = output2 ++ (unlines list)
                         putStrLn output
                         loop state
                  ("/start":who:_) ->
                      do putStrLn ("Must start conversation with " ++ who)
                         loop state
                  ("/stop":"all":_) ->
                      do putStrLn ("Must stop all conversations")
                         loop state
                  ("/stop":who:_) ->
                      do putStrLn ("Must stop conversation with " ++ who)
                         loop state
                  ("/ping":who:_) ->
                      do putStrLn ("Must ping " ++ who)
                         loop state
                  ("/add":who:_) ->
                      do contactList <- atomically $ readTVar (csContactList state)
                         let mContact = contactLookup who contactList
                         if (isNothing mContact) then
                             do
                               -- create "empty" contact to be filled by the next Alive request
                               let newContact = Contact {
                                                  contactUserName = who
                                                , contactStatus   = NotAvailable
                                                , contactIpAddr   = ""
                                                }
                               atomically $ writeTVar (csContactList state) (newContact : contactList)
                               -- save contact list to file
                               _ <- forkIO $ saveContactList state (newContact : contactList)
                               putStrLn (who ++ " was added to contact list.")
                         else
                             putStrLn (who ++ " is already is your contact list !")
                         loop state
                  ("/del":who:_) ->
                      do contactList <- atomically $ readTVar (csContactList state)
                         let mContact = contactLookup who contactList
                         if (isNothing mContact) then
                             putStrLn (who ++ " is not in your contact list !")
                         else
                             do
                               let newContactList = filter ((/= who) . contactUserName) contactList
                               atomically $ writeTVar (csContactList state) newContactList
                               _ <- forkIO $ saveContactList state newContactList
                               putStrLn (who ++ " has been removed from your contact list !")
                         loop state
                  ("/switchto":who:_) ->
                      do putStrLn ("Must switch active conversation to : " ++ who)
                         loop state
                  ("/quit":_) ->
                      do putStrLn ("Must quit")
                         loop state
                  (('/':cmd'):_) ->
                      do putStrLn ("Unknow command : \"/" ++ cmd' ++ "\"")
                         loop state
                  _ -> do putStrLn ("Must write message (check active conversation first) : " ++ unwords cmd)
                          loop state

talkDispatcher :: ClientState -> MVar Bool -> (Context, SockAddr) -> IO ()
talkDispatcher state authSuccess (context, servaddr) = do
  success "success\n\n"

  -- Sending Born request
  putStr "Please identify.\nUsername : "
  username <- getLine
  password <- (B8.pack . fromMaybe "") `fmap` runInputT defaultSettings (getPassword Nothing "Password : ")

  -- Parse contact list
  putStr "Reading contact list..."
  db <- getContactListDb username
  contactListDb <- case db of
                     Left err ->
                         do
                           failed ("failed\n" ++ err ++ "\n")
                           exitFailure
                     Right file ->
                         return file
  success "success\n"

  -- Creating contact list from file
  putStr "Building contact list..."
  raw <- withFile contactListDb ReadMode $ \h ->
         do !contents <- L.hGetContents h
            return contents

  let list = case L.null raw of
               True -> []
               False -> decode raw :: ContactList

  -- Modifying state to take contact list into account
  atomically $ do
    writeTVar (csContactList state) list
    writeTVar (csContactListDb state) contactListDb
  success "success\n"

  putStrLn "\nIdentifying to dispatcher..."
  send context (L.toStrict $ encode (Born username password))

  -- Parsing dispatcher's answer
  mbs <- recv context
  case mbs of
    Nothing -> failed "Lost contact with server.\n"
    Just bs -> do
             let answer = decode (L.fromStrict bs) :: DispatcherAnswer
             case answer of
               -- Auth was okay
               BornOK str ->
                   -- Print success message
                   do success "Identification successful.\n"
                      when (not $ isNothing str) (putStrLn ("Dispatcher notice : " ++ fromJust str))
                      success "You are now connected !\n"

                      -- Put sync variable to True so that 'main' can start listening on user's input
                      putMVar authSuccess True

                      -- fork thread to keep alive
                      putStr "Starting keep-alive routine..."
                      keepAlive_tId <- forkIO $
                         do
                           threadDelay ((10^6) :: Int)
                           forever $ do
                             keepAlive context state
                             threadDelay ((9 * 10^6) :: Int)
                      success "success\n"

                      -- loop to handle client's input (e.g. log out requests)
                      loop state context
                      
               -- Auth failed
               BornKO str ->
                   do failed "Identification failed "
                      when (not $ isNothing str) (putStrLn (": " ++ fromJust str))
                      putMVar authSuccess False -- put sync to False so the 'main' knows not to listen to user's input
               _  -> error "Error : disptacher returned wrong type of answer\n"

      where loop state context = do
                 -- parse commands to send to the dispatcher
                 command <- atomically $ readTChan (csCommands state)
                 case command of
                   ChangeStatus newStatus ->
                       do putStrLn ("TODO : send dispatcher : " ++ show command)
                          loop state context
                   AddContact username ->
                       do putStrLn ("TODO : send dispatcher : " ++ show command)
                          loop state context
                   Quit ->
                       do putStrLn ("TODO : send dispatcher : " ++ show command)
                          loop state context
                   _ -> do failed ("Error : unknown command received : " ++ show command ++ "\n")
                           loop state context
                                            
