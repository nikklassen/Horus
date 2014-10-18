module Calculator.Data.Env (
    Env(..),
    EnvState,
    alterVars,
    alterFuncs
) where

import Calculator.Data.AST
import Calculator.Functions
import Control.Monad.StateStack (StateStack)
import Data.Map (Map, alter)

type EnvState a = StateStack Env a

data Env = Env { getVars :: Map String AST
               , getFuncs :: Map String Function
               } deriving (Show, Eq)

alterVars :: (Maybe AST -> Maybe AST) -> String -> Env -> Env
alterVars f var (Env vars funcs) = Env (alter f var vars) funcs

alterFuncs :: (Maybe Function -> Maybe Function) -> String -> Env -> Env
alterFuncs f funcName (Env vars funcs) = Env vars $ alter f funcName funcs
