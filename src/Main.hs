module Main (main) where

import Client.Connection
import Network.Simple.TCP.TLS
import Network.TLS.Extra (fileReadPrivateKey, fileReadCertificate)
import System.Certificate.X509.Unix
import System.Environment (getArgs)
import Tools

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

-- connect :: ClientSettings -> hostname -> ServiceName -> function

-- | Start client and server parts
hpt :: String -> IO ()
hpt hostname = withSocketsDo $ do
        -- Construct credential information
        putStr "Loading certificate..."
        certificate <- fileReadCertificate certificateFile
        putStr "success\n"
        putStr "Loading private key..."
        privKey     <- fileReadPrivateKey privateKeyFile
        putStr "success\nCreating client credential..."
        let cred = Credential certificate privKey []
        putStr "success\n"
        
        -- Get certificate store
        putStr "Getting client certificate store..."
        cStore <- getSystemCertificateStore
        putStr "success\n"
               
        -- Create client SSL settings
        putStr "Creating client SSL settings..."
        let clientSettings = makeClientSettings [cred] (Just hostname) cStore
--        clientSettings <- getDefaultClientSettings
        putStr "success.\n"

        -- Connecting to the dispatcher
        putStrLn "Connecting to dispatcher..."
        _ <- connect clientSettings hostname "1089" talkDispatcher
        
        putStrLn "\nClient exiting.\n"

talkDispatcher :: (Context, SockAddr) -> IO ()
talkDispatcher (context, servaddr) = do
  putStrLn ("Client connected to dispatcher :\n" ++ show servaddr ++ "\n")
