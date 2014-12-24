{-# LANGUAGE BangPatterns #-}

module Calculator.Evaluator (
    evalPass
) where

import Calculator.Canon
import Calculator.Data.AST
import Calculator.Data.Decimal (Decimal)
import Calculator.Data.Env
import Calculator.Evaluator.Helpers
import Calculator.Data.Function
import Calculator.SynCheck
import Control.Monad.RWS
import qualified Data.Map as Map (empty)

-- Start at the top level scope, i.e. no local vars
evalPass :: AST -> UserPrefs -> Env -> (Decimal, Env)
evalPass ast prefs env = let (a, (Scope _ e), _) =
                                runRWS (evalPass' ast)
                                       prefs
                                       (Scope { localVars = Map.empty
                                           , global = env
                                           })
                         in (a, e)

evalPass' :: AST -> ScopeRWS Decimal
evalPass' ast@(EqlStmt (Var var) e) = do
    !_ <- gets (synCheckPass ast . global)
    val <- eval e
    alterGlobal $ alterVars (\_ -> Just $ Number val) var
    return val

evalPass' ast@(EqlStmt (FuncExpr f parameters) e) = do
    !_ <- gets (synCheckPass ast . global)
    let func = buildFunction parameters $ canonPass e
    alterGlobal $ alterFuncs (\_ -> Just func) f
    return 0

evalPass' ast@(BindStmt (Var var) e) = do
    !_ <- gets (synCheckPass ast . global)
    let rhs = canonPass e
    alterGlobal $ alterVars (\_ -> Just rhs) var
    eval rhs

evalPass' ast = eval ast

alterGlobal :: (Env -> Env) -> ScopeRWS ()
alterGlobal f =
    modify $ \scope ->
        let newGlobal = f $ global scope
        in scope { global = newGlobal }
