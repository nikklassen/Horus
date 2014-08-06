module Calculator.VarCheck.Internal (
    varCheck
) where

import Calculator.Data.AST
import Data.Set

varCheck :: AST -> AST
varCheck (EqlStmt f@(FuncExpr _ ps) rhs) = case getParams ps of
                                               (Left e) -> error e
                                               (Right params) -> EqlStmt f $ astMap (checkUndef params) rhs
varCheck ast = ast

checkUndef :: Set String -> AST -> AST
checkUndef params v@(Var a) = if not (a `member` params) then
                                  error $ "Use of undefined parameter " ++ a
                                  else
                                      v
checkUndef _ ast = ast

getParams :: [AST] -> Either String (Set String)
getParams (Var v:ps) = case getParams ps of
                           e@(Left _) -> e
                           (Right params) ->
                                            if v `member` params then
                                                Left $ "Duplicate parameter " ++ v
                                                else
                                                    Right $ insert v params
getParams [] = Right Data.Set.empty
getParams (ast:_) = Left $ "Unexpected parameter " ++ show ast
