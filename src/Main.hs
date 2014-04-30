{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Client.Connection
import           Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L (toStrict, fromStrict)
import qualified Data.ByteString.Char8 as B8 (putStrLn, getLine)
import           Data.CertificateStore
import           Data.Monoid ((<>))
import           Network.Simple.TCP.TLS
import           Network.TLS
import           Network.TLS.Extra (fileReadPrivateKey, fileReadCertificate)
import           System.Certificate.X509.Unix
import           System.Environment (getArgs)
import           System.IO (hSetBuffering, BufferMode(..), stdout)
import           Tools
import           Types

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

        -- Connecting to the dispatcher
        hSetBuffering stdout NoBuffering
        putStr "Connecting to dispatcher..."
        connect clientSettings hostname port talkDispatcher
        
        putStrLn "\nClient exiting.\n"

talkDispatcher :: (Context, SockAddr) -> IO ()
talkDispatcher (context, servaddr) = do
  success "success\n\n"

  -- Sending Born request
  putStr "Please identify.\nUsername : "
  username <- getLine
  putStr "Password : "
  password <- B.getLine
  putStrLn "\nIdentifying to dispatcher..."
  send context (L.toStrict $ encode (Born username password))
  mbs <- recv context
  case mbs of
    Nothing -> failed "Lost contact with server.\n"
    Just bs -> do
             let answer = decode (L.fromStrict bs) :: DispatcherAnswer
             case answer of
               BornOK Nothing -> do
                           success "Identification successful. You are now connected.\n"
                           loop
               BornOK (Just reason) -> do
                        success "Identification successful.\n"
                        putStrLn ("Dispatcher notice : " <> reason <> "\n")
                        success "You are now connected.\n"
                        loop
               BornKO Nothing -> failed "Identification failed.\n"
               BornKO (Just reason) -> do
                        failed "Identification failed :\n"
                        putStrLn reason
               _  -> error "Error : disptacher returned wrong type of answer\n"

      where loop = do
              putStr "Type in to send to the client : "
              bs <- B8.getLine
              send context bs
              loop
  

