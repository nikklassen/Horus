module Calculator.SynCheck.Internal (
    synCheck
) where

import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.DeepSeq ()
import Control.DeepSeq (force)
import Data.Map ((!))
import Data.Set (Set, insert)
import qualified Data.Map as Map (member)
import qualified Data.Set as Set (empty, member)

-- Catch all for various context-sensitive syntax checks
synCheck :: AST -> Env -> AST
synCheck ast@(EqlStmt (Var v) _) env = if v `Map.member` getBound env then
                                           error $ "Redefinition of bound variable " ++ v
                                       else
                                           ast
synCheck (EqlStmt f@(FuncExpr _ ps) rhs) _ = case getParams ps of
                                                 (Left e) -> error e
                                                 (Right params) -> EqlStmt f $! astMap (checkUndef params) rhs
synCheck (BindStmt (Var b) rhs) env = if b `Map.member` getVars env then
                                          error $ "Redefinition of variable " ++ b
                                      else
                                          astMap (checkForRecursiveDef b env) rhs
synCheck ast _ = ast

checkUndef :: Set String -> AST -> AST
checkUndef params v@(Var a) = if not (a `Set.member` params) then
                                  error $ "Use of undefined parameter " ++ a
                              else
                                  v
checkUndef _ ast = ast

getParams :: [AST] -> Either String (Set String)
getParams (Var v:ps) = case getParams ps of
                           e@(Left _) -> e
                           (Right params) ->
                               if v `Set.member` params then
                                   Left $ "Duplicate parameter " ++ v
                               else
                                   Right $ insert v params
getParams [] = Right Set.empty
getParams (ast:_) = Left $ "Unexpected parameter " ++ show ast

checkForRecursiveDef :: String -> Env -> AST -> AST
checkForRecursiveDef b env@(Env _ _ binds) ast@(Var v)
    | v == b = error $ "Recursive use of bound variable " ++ b
    | v `Map.member` binds = astMap (checkForRecursiveDef b env) $ binds ! v
    | otherwise = ast
checkForRecursiveDef _ _ ast = force ast
