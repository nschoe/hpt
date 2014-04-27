module Main (main) where

import System.Environment (getArgs)
import Dispatcher.Tools
import Dispatcher.Connection
import Network.Simple.TCP.TLS
import Network.TLS.Extra (fileReadPrivateKey, fileReadCertificate)
import Text.Printf (printf)

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
  certificate <- fileReadCertificate certificateFile
  putStr "success\n"
  putStr "Loading private key..."
  privKey     <- fileReadPrivateKey privateKeyFile
  putStr "success\nCreating server credential..."
  let cred = Credential certificate privKey []
  putStr "success\n"

  -- Create server SSL settings
  putStr "Creating server SSL settings..."
  let servSettings = makeServerSettings cred Nothing
  putStr "success\n"

  -- Start the server
  printf "Starting dispatcher on port %s...\n" port
  serve servSettings (Host "localhost") port handleConnection

  putStrLn "Server exiting..."

-- | Deal with a client connecting
handleConnection :: (Context, SockAddr) -> IO()
handleConnection (context, sockaddr) = do
  putStrLn ("Received connection :\n>>>>>> " ++ show sockaddr ++ "\n<<<<<<\n\n")
