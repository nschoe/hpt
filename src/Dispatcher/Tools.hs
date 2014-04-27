module Dispatcher.Tools (
                          usage
                        , version
                        ) where

-- | Define some useful functions and wrappers for hpt

-- | Classic usage string when user lauched with wrong arguments
usage :: String
usage = concat [ "Usage : \t-h, --help : display this message.\n"
               , "\t\t-v, --version : display hpt-dispatcher's version and stability.\n"
               , "\t\t-s, --start : start the dispatcher server.\n"
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
