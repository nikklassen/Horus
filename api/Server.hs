module Main where

import TextToMath
import Calculator.Functions (Function(..))
import Calculator.Data.AST (AST(..))

import Happstack.Server
import Data.Acid
import UserState
import Control.Exception
import Data.Acid.Memory

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
                   TextToMath.app acid)
