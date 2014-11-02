module Calculator (
    calculate,
    module Calculator.Data.Result
) where

import Calculator.Data.AST
import Calculator.Data.Decimal
import Calculator.Data.Env
import Calculator.Data.Result
import Calculator.Evaluator
import Calculator.Parser
import Data.Map (Map)
import qualified Data.Map as Map (insert, mapAccumWithKey, empty)

calculate :: String -> Env -> Result
calculate eq env = let (r, newEnv@(Env vs fs)) = evalPass (parse eq) env
                       bound = fst $ Map.mapAccumWithKey (evalBound newEnv) Map.empty vs
                   in Result r vs fs bound

evalBound :: Env -> Map String Decimal -> String -> AST -> (Map String Decimal, AST)
evalBound _ a _ n@(Number _) = (a, n)
evalBound env a v ast = (Map.insert v (fst $ evalPass ast env) a, ast)
