module Dispatcher.Tools (
                          usage
                        , version
                        , success
                        , failed
                        , isBornRequest
                        , saveDb
                        ) where

import           System.Console.ANSI (setSGR, ConsoleLayer(Foreground), ColorIntensity(Vivid), Color(Green, Red), SGR(Reset, SetColor))
import           Types (DispatcherRequest(..), DispatcherState(..))
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as L (toStrict)
import Data.ByteString (hPut)
import System.IO (IOMode(..), withFile)


-- | Define some useful functions and wrappers for hpt

-- | Classic usage string when user lauched with wrong arguments
usage :: String
usage = concat [ "Usage : \t-h, --help : display this message.\n"
               , "\t\t-v, --version : display hpt-dispatcher's version and stability.\n"
               , "\t\t-s, --start <port> : start the dispatcher server, listening on port <port>.\n"
               ]

-- | Version and stability information about hpt
version :: String
version = concat [ "hpt-dispatcher - Haskell Private Talk Dispatcher version 0.1.0\n"
                 , "==============================================================\n"
                 , "Stability : experimental\n"
                 , "hpt in under development and no garantee is given that the API will have consistency.\n\
                   \Please use for development and non-critical applications only.\n"
                 , "Author : Nicolas Schoemaeker (nschoe) <ns.schoe@gmail.com>\n"
                 ]

-- | Eye-candy terminal formatting to print "success" in green
success :: String -> IO ()
success str = do
  setSGR [SetColor Foreground Vivid Green]
  putStr str
  setSGR [Reset]

-- | Eye-candy terminal formatting to print failed" in red
failed :: String -> IO ()
failed str = do
  setSGR [SetColor Foreground Vivid Red]
  putStr str
  setSGR [Reset]

-- | Simple helper function to match Born requests from a DispatcherRequest
isBornRequest :: DispatcherRequest -> Bool
isBornRequest (Born _ _) = True
isBornRequest _          = False

-- | Saves the Map of registered users on file
saveDb :: DispatcherState -> FilePath -> IO ()
saveDb state regDb = do
  -- get map
  regmap <- atomically $ readTVar (dsRegistered state)

  -- write it to file
  withFile regDb WriteMode $ \h ->
      do hPut h (L.toStrict (encode regmap))
