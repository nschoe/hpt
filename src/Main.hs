module Main (main) where

import System.Environment (getArgs)
import Tools (version, usage)

{- | hpt main function.
   Start the client and the server parts of hpt.
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
