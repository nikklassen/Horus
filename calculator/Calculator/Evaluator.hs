module Calculator.Evaluator (
    evalPass
) where

import Calculator.Canon
import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.Data.Function
import Calculator.Data.Result
import Calculator.Error
import Calculator.Evaluator.Helpers
import Control.Monad.RWS
import qualified Data.Map as Map (empty)

-- Start at the top level scope, i.e. no local vars
evalPass :: AST -> UserPrefs -> Env -> Safe (Result, Env)
evalPass ast prefs env = do
    (res, Scope _ e, _) <- runRWST (evalPass' ast) prefs scope
    return (res, e)
    where scope = Scope { localVars = Map.empty
                        , global = env
                        }

evalPass' :: AST -> ScopeRWS Result
evalPass' (EqlStmt (Var var) e) = do
    val <- eval e
    alterGlobal $ alterVars (\_ -> Just $ Number val) var
    return VarResult {
        answer = val,
        name = var,
        value = Number val,
        boundResults = Map.empty
    }


evalPass' (EqlStmt (FuncExpr f parameters) e) = do
    astCanon <- lift $ canonPass e
    let func = buildFunction parameters astCanon
    return FuncResult {
        name = f,
        def = func
    }

evalPass' (BindStmt (Var var) e) = do
    rhs <- lift $ canonPass e
    alterGlobal $ alterVars (\_ -> Just rhs) var
    val <- eval rhs
    return VarResult {
        answer = val,
        name = var,
        value = rhs,
        boundResults = Map.empty
    }

evalPass' ast = CalcResult <$> eval ast

alterGlobal :: (Env -> Env) -> ScopeRWS ()
alterGlobal f =
    modify $ \scope ->
        let newGlobal = f $ global scope
        in scope { global = newGlobal }
