{-# LANGUAGE OverloadedStrings #-}

module Main where

import Calculator.Data.AST (AST(..))
import Calculator.Functions (Function(..))
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
                    (Map.fromList [("a", 2)])
                    (Map.fromList [("a", a)])
                    (Map.fromList [("y", Var "a")]))
                , ("testUser2", User
                    (Map.fromList [("b", 3)])
                    (Map.fromList [("b", b)])
                    (Map.fromList [("z", Var "b")]))
                ]
            where a = Function ["x"] (OpExpr "+" (Number 2) (Var "x"))
                  b = Function ["x"] (OpExpr "+" (Number 3) (Var "x"))
