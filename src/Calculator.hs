module Calculator where

import Calculator.Parser
import Calculator.Evaluator
import Data.Number.CReal
import Data.Map (Map, empty)
import Control.Monad.State

data Result = Result {
    answer :: Maybe CReal,
    vars :: Map String CReal
} deriving (Eq, Show)

-- TODO take out empty function map
calculate :: String -> Map String CReal -> Result
calculate eq varMap = let (r, Env vs _) = runState (eval (parse eq)) $ Env varMap empty
                      in Result (Just r) vs
