module Calculator.ASTPass.SynCheck (
    synCheckPass
) where

import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.Data.Function (body)
import Calculator.Error
import Control.Monad.Except
import Data.Map ((!))
import Data.Set (Set, insert)
import qualified Data.Map as Map (member, notMember)
import qualified Data.Set as Set (empty, member, notMember)

-- Catch all for various context-sensitive syntax checks
synCheckPass :: Env -> AST -> Safe AST
synCheckPass env ast@(EqlStmt lhs rhs) =
        case lhs of
            (FuncExpr fName ps) -> do
                params <- getParams ps
                let funcCheck = checkForRecursiveFunc fName env >=> checkUndef params env
                _ <- astMap funcCheck rhs
                return ast
            (Var _) -> return ast

            -- This case will caught by the parser except in cases of
            -- inlined variables (i.e. pi, e)
            _ -> throwError $ "Cannot assign value to " ++ show lhs

synCheckPass env ast@(BindStmt lhs rhs) =
        case lhs of
            (Var b) -> astMap (checkForRecursiveDef b env) rhs >> return ast
            _ -> throwError $ "Cannot bind value to " ++ show lhs

synCheckPass _ ast = return ast

checkUndef :: Set String -> Env -> AST -> Safe AST
checkUndef params env v@(Var a) = if a `Set.notMember` params && a `Map.notMember` getVars env then
                                      throwError $ "Use of undefined parameter " ++ a
                                  else
                                      return v
checkUndef _ _ ast = return ast

getParams :: [AST] -> Safe (Set String)
getParams (Var v:ps) = do
        params <- getParams ps
        if v `Set.member` params then
            throwError $ "Duplicate parameter " ++ v
        else
            return $ insert v params
getParams [] = return Set.empty
getParams (ast:_) = throwError $ "Unexpected parameter " ++ show ast

checkForRecursiveDef :: String -> Env -> AST -> Safe AST
checkForRecursiveDef newVar env@(Env vars _) ast@(Var v)
    | v == newVar = throwError $ "Recursive use of bound variable " ++ newVar
    | v `Map.member` vars = astMap (checkForRecursiveDef newVar env) (vars ! v) >> return ast
    | otherwise = return ast
checkForRecursiveDef _ _ ast = return ast

checkForRecursiveFunc :: String -> Env -> AST -> Safe AST
checkForRecursiveFunc newFunc env@(Env _ funcs) ast@(FuncExpr f _)
    | f == newFunc = throwError $ "Recursive use of function " ++ newFunc
    | f `Map.member` funcs = astMap (checkForRecursiveFunc newFunc env) (body $ funcs ! f) >> return ast
    | otherwise = return ast
checkForRecursiveFunc _ _ ast = return ast
