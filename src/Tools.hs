module Tools (
              usage
             , version
             ) where

-- | Define some useful functions and wrappers for hpt

-- | Classic usage string when user lauched with wrong arguments
usage :: String
usage = concat [ "Usage : \t-h, --help : display this message.\n"
               , "\t\t-v, --version : display hpt's version and stability.\n"
               ]

-- | Version and stability information about hpt
version :: String
version = concat [ "hpt - Haskell Private Talk version 0.1.0\n"
                 , "========================================\n"
                 , "Stability : experimental\n"
                 , "hpt in under development and no garantee is given that the API will have consistency.\n\
                   \Please use for development and non-critical applications only.\n"
                 , "Author : Nicolas Schoemaeker (nschoe) <ns.schoe@gmail.com>\n"
                 ]
