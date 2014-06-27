{-# LANGUAGE OverloadedStrings #-}

module Main where

import TextToMath
import Calculator.Functions (Function(..))
import Calculator.Data.AST (AST(..))
import qualified Data.Map as Map (fromList)
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
main = bracket (openMemoryState testState)
               closeAcidState
               (\acid -> simpleHTTP config $ do
                   decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
                   TextToMath.app acid)

testState :: UserDb
testState = UserDb $ Map.fromList
                [ ("testUser", User
                    (Map.fromList [("a", "2.0")])
                    (Map.fromList [("a", a)]))
                , ("testUser2", User
                    (Map.fromList [("b", "3.0")])
                    (Map.fromList [("b", b)]))
                ]
            where a = Function ["x"] (OpExpr "+" (Var "x") (Number "2"))
                  b = Function ["x"] (OpExpr "+" (Var "x") (Number "3"))
