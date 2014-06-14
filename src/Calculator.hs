module Calculator where

import Calculator.Parser
import Calculator.Evaluator
import Calculator.Functions

import Data.Number.CReal
import Data.Map (Map)
import Control.Monad.State

data Result = Result {
    answer :: Maybe CReal,
    vars :: Map String CReal,
    funcs :: Map String Function
} deriving (Eq, Show)

calculate :: String -> Map String CReal -> Map String Function -> Result
calculate eq varMap funcMap = let (r, Env vs fs) = runState (eval (parse eq)) $ Env varMap funcMap
                              in Result (Just r) vs fs
