-- These functions act as the core of the evaluation engine, but should not
-- mutate the environment.  Defining statements are handled by the evalPass

module Calculator.Evaluator.Helpers (
    eval,
    operate
) where

import Calculator.Data.AST
import Calculator.Data.Decimal
import Calculator.Data.Env
import Calculator.Data.Function
import Calculator.Error
import Calculator.Functions
import Control.Monad.RWS
import qualified Data.Map as Map (fromList)

eval :: AST -> ScopeRWS Decimal
eval (Number n) = return n

eval (Var var) = getEnvVar var >>= eval

eval (Neg e) = negate <$> eval e

eval (OpExpr op leftExpr rightExpr) = do
    leftVal <- eval leftExpr
    rightVal <- eval rightExpr
    lift $ operate op leftVal rightVal

eval (FuncExpr func es) = do
    args <- mapM eval es
    prefs <- ask
    case getFunction func prefs of
        Just f -> lift $ f args
        Nothing -> getEnvFunc func >>= \f -> evalFunction f args

eval ast = throwError $ "Cannot evaluate the statement " ++ show ast

evalFunction :: Function -> [Decimal] -> ScopeRWS Decimal
evalFunction (Function p b) args = do
    argVars <- lift $ zip' p $ map Number args
    scope <- get
    put $ scope { localVars = Map.fromList argVars }
    res <- eval b
    put scope
    return res

zip' :: [a] -> [b] -> Safe [(a, b)]
zip' (x:xs) (y:ys) = liftM ((:) (x, y)) $ zip' xs ys
zip' [] [] = return []
zip' _ _ = throwError "Unexpected number of arguments"

operate :: String -> Decimal -> Decimal -> Safe Decimal
operate op n1 n2 =
    if op `notElem` ["+", "*", "-", "/", "^", "%"] then
        throwError $ "Use of unsupported operator " ++ op
    else return $ case op of
        "+" -> n1 + n2
        "*" -> n1 * n2
        "-" -> n1 - n2
        "/" -> n1 / n2
        "^" -> let sign = n1 / abs n1
               in sign * (abs n1 ** n2)
        "%" -> realMod n1 n2
        _ -> error "Missing supported operator"
    where realMod a b = a - fromInteger (floor $ a/b) * b
