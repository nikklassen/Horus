module Main where

import Control.Exception
import Data.Acid
import Happstack.Server
import Horus
import UserState

config :: Conf
config = Conf { port        = 3000
              , validator   = Nothing
              , logAccess   = Just logMAccess
              , timeout     = 30
              , threadGroup = Nothing
              }

main :: IO ()
main = bracket (openLocalState UserState.emptyState)
       closeAcidState
       (\acid -> simpleHTTP config $ do
           decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
           Horus.app acid)
