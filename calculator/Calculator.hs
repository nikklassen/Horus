module Calculator (
    calculate,
    module Calculator.Data.Result
) where

import Calculator.Data.Env
import Calculator.Data.Result
import Calculator.Evaluator
import Calculator.Parser
import qualified Data.Map as Map (map)

calculate :: String -> Env -> Result
calculate eq env = let (r, newEnv@(Env vs fs bs)) = evalPass (parse eq) env
                       bsResults = Map.map (\e -> (fst (evalPass e newEnv), e)) bs
                   in Result r vs fs bsResults
