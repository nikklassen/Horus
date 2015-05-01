{-# LANGUAGE OverloadedStrings #-}

module Main where

import Calculator.Data.AST (AST(..))
import Calculator.Data.Env (defaultPrefs)
import Calculator.Data.Function (Function(..))
import Calculator.Parser
import Control.Exception
import Data.Acid
import Data.Acid.Memory
import Happstack.Server
import Horus
import UserState
import qualified Data.Map as Map (fromList)

config :: Conf
config = Conf { port        = 3000
              , validator   = Nothing
              , logAccess   = Just logMAccess
              , timeout     = 30
              , threadGroup = Nothing
              }

main :: IO ()
main = bracket (openMemoryState testState)
               closeAcidState
               (\acid -> simpleHTTP config $ do
                   decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
                   Horus.app acid)

testState :: UserDb
testState = UserDb $ Map.fromList
                [ ("testUser", User
                    (Map.fromList [("a", (Number 2, "")), ("y", (Var "a", "a"))])
                    (Map.fromList [("a", a)])
                    defaultPrefs)
                , ("testUser2", User
                    (Map.fromList [("b", (Number 3, "")), ("z", (Var "b", "b"))])
                    (Map.fromList [("b", b)])
                    defaultPrefs)
                ]
            where a = (Function ["x"] [m|2 + x|], "x + 2")
                  b = (Function ["x"] [m|3 + x|], "x + 3")
