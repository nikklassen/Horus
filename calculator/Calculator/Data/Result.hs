module Calculator.Data.Result (
    Result(..)
) where

import Calculator.Data.AST
import Calculator.Functions
import Data.Map (Map)
import Data.Number.CReal

data Result = Result {
    answer :: CReal,
    vars :: Map String AST,
    funcs :: Map String Function,
    boundResults :: Map String CReal
} deriving (Eq, Show)
