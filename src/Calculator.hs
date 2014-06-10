module Calculator where

import Calculator.Parser
import Calculator.Evaluator
import Data.Number.CReal
import Data.Map (Map)
import Control.Monad.State

data Result = Result {
    answer :: Maybe CReal,
    vars :: Map String CReal
} deriving (Eq, Show)

calculate :: String -> Map String CReal -> Result
calculate eq varMap = let (r, vs) = runState (eval (parse eq)) varMap
                    in Result (Just r) vs
