module Calculator (
    calculate,
    module Calculator.Data.Result
) where

import Calculator.ASTPass
import Calculator.Data.AST
import Calculator.Data.Decimal
import Calculator.Data.Env
import Calculator.Data.Result
import Calculator.Evaluator
import Calculator.Parser
import Data.Map (Map)
import qualified Data.Map as Map (insert, mapAccumWithKey, empty)

calculate :: String -> UserPrefs -> Env -> Result
calculate eq prefs env = let (r, newEnv@(Env vs fs)) = evalPass (runASTPasses env $ parse eq) prefs env
                             bound = fst $ Map.mapAccumWithKey (evalBound prefs newEnv) Map.empty vs
                         in Result r vs fs bound

evalBound :: UserPrefs -> Env -> Map String Decimal -> String -> AST -> (Map String Decimal, AST)
evalBound _ _ a _ n@(Number _) = (a, n)
evalBound prefs env a v ast = (Map.insert v (fst $ evalPass ast prefs env) a, ast)
