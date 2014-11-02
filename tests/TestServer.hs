{-# LANGUAGE OverloadedStrings #-}

module Main where

import Calculator.Data.AST (AST(..))
import Calculator.Functions (Function(..))
import Calculator.Parser
import Control.Exception
import Data.Acid
import Data.Acid.Memory
import Happstack.Server
import TextToMath
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
                   TextToMath.app acid)

testState :: UserDb
testState = UserDb $ Map.fromList
                [ ("testUser", User
                    (Map.fromList [("a", Number 2), ("y", Var "a")])
                    (Map.fromList [("a", a)]))
                , ("testUser2", User
                    (Map.fromList [("b", Number 3), ("z", Var "b")])
                    (Map.fromList [("b", b)]))
                ]
            where a = (Function ["x"] [m|2 + x|], "x + 2")
                  b = (Function ["x"] [m|3 + x|], "x + 3")
