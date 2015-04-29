module Calculator.Evaluator (
    evalPass
) where

import Calculator.Canon
import Calculator.Data.AST
import Calculator.Data.Decimal (Decimal)
import Calculator.Data.Env
import Calculator.Data.Function
import Calculator.Error
import Calculator.Evaluator.Helpers
import Control.Monad.RWS
import qualified Data.Map as Map (empty)

-- Start at the top level scope, i.e. no local vars
evalPass :: AST -> UserPrefs -> Env -> Safe (Decimal, Env)
evalPass ast prefs env = do
    (a, Scope _ e, _) <- runRWST (evalPass' ast) prefs scope
    return (a, e)
    where scope = Scope { localVars = Map.empty
                        , global = env
                        }

evalPass' :: AST -> ScopeRWS Decimal
evalPass' (EqlStmt (Var var) e) = do
    val <- eval e
    alterGlobal $ alterVars (\_ -> Just $ Number val) var
    return val

evalPass' (EqlStmt (FuncExpr f parameters) e) = do
    astCanon <- lift $ canonPass e
    let func = buildFunction parameters astCanon
    alterGlobal $ alterFuncs (\_ -> Just func) f
    return 0

evalPass' (BindStmt (Var var) e) = do
    rhs <- lift $ canonPass e
    alterGlobal $ alterVars (\_ -> Just rhs) var
    eval rhs

evalPass' ast = eval ast

alterGlobal :: (Env -> Env) -> ScopeRWS ()
alterGlobal f =
    modify $ \scope ->
        let newGlobal = f $ global scope
        in scope { global = newGlobal }
