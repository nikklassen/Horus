module Calculator where

import Calculator.Evaluator
import Calculator.Canon
import Calculator.Functions
import Calculator.Parser
import Data.Number.CReal
import Data.Map (Map)

data Result = Result {
    answer :: CReal,
    vars :: Map String CReal,
    funcs :: Map String Function
} deriving (Eq, Show)

calculate :: String -> Map String CReal -> Map String Function -> Result
calculate eq varMap funcMap = let (r, Env vs fs) = evalPass (canonPass $ parse eq) varMap funcMap
                              in Result r vs fs
