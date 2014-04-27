module Client.Connection (
                               port
                             , privateKeyFile
                             , certificateFile
                             ) where

-- | Port thatthe dispatcher listens to for incoming connection
port :: String
port = "1089"

-- | Path to the file containing the private key
privateKeyFile :: FilePath
privateKeyFile = "/home/nschoe/workspace/haskell/hpt/src/Client/ssl/privkey.pem"

-- | Path to the file containing the x509 certificate
certificateFile :: FilePath
certificateFile = "/home/nschoe/workspace/haskell/hpt/src/Client/ssl/cacert.pem"
