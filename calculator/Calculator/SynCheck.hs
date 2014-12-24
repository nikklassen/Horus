module Calculator.SynCheck (
    synCheckPass
) where

import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.Data.Function (body)
import Control.DeepSeq (force, ($!!))
import Data.Map ((!))
import Data.Set (Set, insert)
import qualified Data.Map as Map (member, notMember)
import qualified Data.Set as Set (empty, member, notMember)

-- Catch all for various context-sensitive syntax checks
synCheckPass :: AST -> Env -> AST
synCheckPass (EqlStmt f@(FuncExpr fName ps) rhs) env =
        case getParams ps of
            (Left e) -> error e
            (Right params) ->
                let funcCheck = checkForRecursiveFunc fName env . checkUndef params env
                in EqlStmt f $!! astMap funcCheck rhs
synCheckPass (BindStmt (Var b) rhs) env = astMap (checkForRecursiveDef b env) rhs
synCheckPass ast _ = ast

checkUndef :: Set String -> Env -> AST -> AST
checkUndef params env v@(Var a) = if a `Set.notMember` params && a `Map.notMember` getVars env then
                                      error $ "Use of undefined parameter " ++ a
                                  else
                                      v
checkUndef _ _ ast = ast

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
checkForRecursiveDef newVar env@(Env vars _) ast@(Var v)
    | v == newVar = error $ "Recursive use of bound variable " ++ newVar
    | v `Map.member` vars = astMap (checkForRecursiveDef newVar env) $ vars ! v
    | otherwise = ast
checkForRecursiveDef _ _ ast = force ast

checkForRecursiveFunc :: String -> Env -> AST -> AST
checkForRecursiveFunc newFunc env@(Env _ funcs) ast@(FuncExpr f _)
    | f == newFunc = error $ "Recursive use of function " ++ newFunc
    | f `Map.member` funcs = astMap (checkForRecursiveFunc newFunc env) $ body $ funcs ! f
    | otherwise = ast
checkForRecursiveFunc _ _ ast = force ast
