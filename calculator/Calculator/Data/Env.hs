module Calculator.Data.Env (
    Env(..),
    EnvState,
    alterVars,
    alterFuncs,
    alterBound
) where

import Calculator.Data.AST
import Calculator.Functions
import Control.Monad.State (State)
import Data.Map (Map, alter)
import Data.Number.CReal (CReal)

type EnvState = State Env

data Env = Env { getVars :: Map String CReal
               , getFuncs :: Map String Function
               , getBound :: Map String AST
               } deriving (Show, Eq)

alterVars :: (Maybe CReal -> Maybe CReal) -> String -> Env -> Env
alterVars f var (Env vars funcs bound) = Env (alter f var vars) funcs bound

alterFuncs :: (Maybe Function -> Maybe Function) -> String -> Env -> Env
alterFuncs f func (Env vars funcs bound) = Env vars (alter f func funcs) bound

alterBound :: (Maybe AST -> Maybe AST) -> String -> Env -> Env
alterBound f boundVar (Env vars funcs bound) = Env vars funcs (alter f boundVar bound)
