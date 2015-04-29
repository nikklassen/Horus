{-# LANGUAGE DoAndIfThenElse, RecordWildCards #-}

module Calculator.Data.Env where

import Calculator.Data.AST
import Calculator.Data.Function
import Control.Monad.Except
import Control.Monad.RWS (get, gets)
import Control.Monad.Trans.RWS (RWST)
import Data.Map (Map, alter, member, (!))
import Data.SafeCopy

type ScopeRWS = RWST UserPrefs String Scope (Except String)

data UserPrefs = UserPrefs { isRadians :: Bool
                           } deriving (Show)

defaultPrefs :: UserPrefs
defaultPrefs = UserPrefs { isRadians = True
                         }

data Env = Env { getVars :: Map String AST
               , getFuncs :: Map String Function
               } deriving (Show, Eq)

alterVars :: (Maybe AST -> Maybe AST) -> String -> Env -> Env
alterVars f var (Env vars funcs) = Env (alter f var vars) funcs

alterFuncs :: (Maybe Function -> Maybe Function) -> String -> Env -> Env
alterFuncs f funcName (Env vars funcs) = Env vars $ alter f funcName funcs

data Scope = Scope { localVars :: Map String AST
                   , global :: Env
                   }

getEnvVar :: String -> ScopeRWS AST
getEnvVar v = do
    Scope{..} <- get
    let globalVars = getVars global
    if v `member` localVars then
        return (localVars ! v)
    else if v `member` globalVars then
        return (globalVars ! v)
    else
        throwError $ "Use of undefined variable \"" ++ v ++ "\""

getEnvFunc :: String -> ScopeRWS Function
getEnvFunc f = do
    globalFuncs <- gets (getFuncs . global)
    if f `member` globalFuncs then
        return (globalFuncs ! f)
    else
        throwError $ "Use of undefined function \"" ++ f ++ "\""

deriveSafeCopy 0 'base ''UserPrefs
