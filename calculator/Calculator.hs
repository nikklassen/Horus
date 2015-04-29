module Calculator (
    calculate,
    module Calculator.Data.Result
) where

import Calculator.ASTPass
import Calculator.Data.AST
import Calculator.Data.Decimal
import Calculator.Data.Env
import Calculator.Data.Result
import Calculator.Error
import Calculator.Evaluator
import Calculator.Parser
import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map (insert, assocs, empty)

calculate :: String -> UserPrefs -> Env -> Safe Result
calculate eq prefs env = do
    checkedAST <- runASTPasses env $ parse eq
    (r, newEnv@(Env vs fs)) <- evalPass checkedAST prefs env
    bound <- foldM (evalBound prefs newEnv) Map.empty (Map.assocs vs)
    return $ Result r vs fs bound

evalBound :: UserPrefs
          -> Env
          -> Map String Decimal         -- the accumulated list of bound results
          -> (String, AST)              -- the variable to evaluate
          -> Safe (Map String Decimal)  -- the evaluated results, will fail if one computation fails
evalBound _ _ acc (_, Number _) = return acc
evalBound prefs env acc (v, ast) = (fst <$> evalPass ast prefs env) >>= \d -> return $ Map.insert v d acc
