module Calculator.Data.Result (
    Result(..)
) where

import Calculator.Data.AST
import Calculator.Data.Function
import Control.DeepSeq (NFData, rnf)
import Data.Map (Map)
import Calculator.Data.Decimal

data Result = Result {
    answer :: Decimal,
    vars :: Map String AST,
    funcs :: Map String Function,
    boundResults :: Map String Decimal
} deriving (Eq, Show)

instance NFData Result where
    rnf (Result r v f b) = rnf r `seq` rnf v `seq` rnf f `seq` rnf b
