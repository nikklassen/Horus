module Calculator.Evaluator (
    eval
) where

import Calculator.Data.AST
import Calculator.Functions

import Data.Number.CReal
import Data.Map (Map)
import qualified Data.Map as Map (alter, lookup)
import Control.Monad.State

type VarState = State (Map String CReal)

eval :: AST -> VarState CReal
eval (Number val) = return $ toNumber val

eval (Var var) = do
    vars <- get
    case Map.lookup var vars of
        Just val -> return val
        Nothing -> error $ "Use of undefined variable \"" ++ var ++ "\""

eval (Neg e) = eval e >>= return . negate

eval (OpExpr op leftExpr rightExpr) = do
    leftVal <- eval leftExpr
    rightVal <- eval rightExpr
    return $ operate op leftVal rightVal

eval (Function func e) = case getFunction func of
    Just f -> eval e >>= return . f
    Nothing -> error $ "Use of undefined function \"" ++ func ++ "\""

eval (EqlStmt (Var var) e) = do
    val <- eval e
    vars <- get
    put $ Map.alter (\_ -> Just val) var vars
    return val

eval ast = error $ "Unexpected AST " ++ show ast

toNumber :: String -> CReal
toNumber ('.':xs) = toNumber $ "0." ++ xs
toNumber num
    | 'e' `elem` num =
        let (a, b) = break (=='e') num
            base = read a
            ex = read $ tail b
        in base * 10**ex
    | otherwise = read num

operate :: String -> CReal -> CReal -> CReal
operate op n1 n2 =
    case op of
        "+" -> n1 + n2
        "*" -> n1 * n2
        "-" -> n1 - n2
        "/" -> n1 / n2
        "^" -> n1 ** n2
        "%" -> realMod n1 n2
        o -> error $ "Unimplemented operator " ++ o
    where realMod a b = a - (fromInteger $ floor $ a/b) * b
