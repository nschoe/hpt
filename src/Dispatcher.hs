module Main (main) where

import System.Environment (getArgs)
import Dispatcher.Tools (version, usage)
import Network.Simple.TCP.TLS

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
      -- short options
      go ("-v":_) = go ["--version"]
      go ("-h":_) = go ["--help"]
      go _        = putStrLn ("Wrong argument.\n" ++ usage)

--serve :: ServerSettings -> HostPreference -> Service Name -> ((Context, SockAddr) -> IO()) -> m a

server = do
  
