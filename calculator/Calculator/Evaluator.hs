{-# LANGUAGE BangPatterns #-}

module Calculator.Evaluator (
    evalPass
) where

import Calculator.Canon
import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.Evaluator.Helpers
import Calculator.Functions
import Calculator.SynCheck
import Control.Monad.State (modify, gets)
import Control.Monad.StateStack (save, restore, runStateStack)
import Data.Number.CReal

evalPass :: AST -> Env -> (CReal, Env)
-- save the initial global state onto the stack
evalPass ast = runStateStack (save >> evalPass' ast)

evalPass' :: AST -> EnvState CReal
evalPass' ast@(EqlStmt (Var var) e) = do
    !_ <- gets $ synCheckPass ast
    val <- eval e
    modifyAndSave $ alterVars (\_ -> Just $ Number val) var
    save
    return val

evalPass' ast@(EqlStmt (FuncExpr f parameters) e) = do
    !_ <- gets $ synCheckPass ast
    let func = buildFunction parameters $ canonPass e
    modifyAndSave $ alterFuncs (\_ -> Just func) f
    return 0

evalPass' ast@(BindStmt (Var var) e) = do
    !_ <- gets $ synCheckPass ast
    let rhs = canonPass e
    modifyAndSave $ alterVars (\_ -> Just rhs) var
    eval rhs

evalPass' ast = eval ast

modifyAndSave :: (Env -> Env) -> EnvState ()
modifyAndSave e = restore >> modify e >> save
