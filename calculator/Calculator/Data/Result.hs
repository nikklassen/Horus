module Calculator.Data.Result (
    Result(..)
) where

import Calculator.Data.AST
import Calculator.Functions
import Data.Map (Map)
import Data.Number.CReal

data Result = Result {
    answer :: CReal,
    vars :: Map String CReal,
    funcs :: Map String Function,
    boundVars :: Map String (CReal, AST)
} deriving (Eq, Show)
