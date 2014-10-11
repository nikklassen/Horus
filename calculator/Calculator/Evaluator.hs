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
import Control.Monad.State (modify, runState, gets)
import Data.Number.CReal

evalPass :: AST -> Env -> (CReal, Env)
evalPass ast = runState (evalPass' ast)

evalPass' :: AST -> EnvState CReal
evalPass' ast@(EqlStmt (Var var) e) = do
    !_ <- gets $ synCheckPass ast
    val <- eval e
    modify $ alterVars (\_ -> Just val) var
    return val

evalPass' ast@(EqlStmt (FuncExpr f parameters) e) = do
    !_ <- gets $ synCheckPass ast
    let func = buildFunction parameters $ canonPass e
    modify $ alterFuncs (\_ -> Just func) f
    return 0

evalPass' ast@(BindStmt (Var iden) e) = do
    !_ <- gets $ synCheckPass ast
    let rhs = canonPass e
    modify $ alterBound (\_ -> Just rhs) iden
    eval rhs

evalPass' ast = eval ast
