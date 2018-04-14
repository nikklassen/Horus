module Calculator (
    calculate,
    calculateBound,
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
    (r, newEnv) <- evalPass checkedAST prefs env
    case r of
        v@VarResult{} -> do
            bound <- calculateBound prefs newEnv
            return v { boundResults = bound }
        res -> return res


calculateBound :: UserPrefs -> Env -> Safe (Map String Decimal)
calculateBound prefs env = foldM (evalBound prefs env) Map.empty (Map.assocs $ getVars env)

evalBound :: UserPrefs
          -> Env
          -> Map String Decimal         -- the accumulated list of bound results
          -> (String, AST)              -- the variable to evaluate
          -> Safe (Map String Decimal)  -- the evaluated results, will fail if one computation fails
evalBound _ _ acc (_, Number _) = return acc
evalBound prefs env acc (v, ast) = ((answer . fst) <$> evalPass ast prefs env) >>= \d -> return $ Map.insert v d acc
