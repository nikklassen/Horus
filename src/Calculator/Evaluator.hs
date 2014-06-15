module Calculator.Evaluator (
    Env(..),
    eval
) where

import Calculator.Data.AST
import Calculator.Functions

import Data.Number.CReal
import Data.Map (Map)
import qualified Data.Map as Map (alter, lookup, fromList, empty)
import Control.Monad.State
import Control.Applicative ((<$>))

data Env = Env { getVars :: Map String CReal
               , getFuncs :: Map String Function
               } deriving (Show, Eq)

type EnvState = State Env

eval :: AST -> EnvState CReal
eval (Number val) = return $ toNumber val

eval (Var var) = do
    vars <- getVars <$> get
    case Map.lookup var vars of
        Just val -> return val
        Nothing -> error $ "Use of undefined variable \"" ++ var ++ "\""

eval (Neg e) = negate <$> eval e

eval (OpExpr op leftExpr rightExpr) = do
    leftVal <- eval leftExpr
    rightVal <- eval rightExpr
    return $ operate op leftVal rightVal

eval (FuncExpr func es) = do
    args <- mapM eval es
    case getFunction func of
        Just f -> return $ f args
        Nothing -> do
            funcs <- getFuncs <$> get
            case Map.lookup func funcs of
                Just f -> return $ evalFunction f args
                Nothing -> error $ "Use of undefined function \"" ++ func ++ "\""

eval (EqlStmt (Var var) e) = do
    val <- eval e
    Env vars funcs <- get
    put $ Env (Map.alter (\_ -> Just val) var vars) funcs
    return val

eval (EqlStmt (FuncExpr f parameters) e) = do
    Env vars funcs <- get
    let func = buildFunction parameters e
    put $ Env vars (Map.alter (\_ -> Just func) f funcs)
    return 0

eval ast = error $ "Unexpected AST " ++ show ast

evalFunction :: Function -> [CReal] -> CReal
evalFunction (Function p b) args =
    let env = Env (Map.fromList $ zip' p args) Map.empty
    in evalState (eval b) env

zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
zip' [] [] = []
zip' _ _ = error "Unexpected number of arguments"

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
        "^" -> let sign = n1 / abs(n1)
               in sign * (abs(n1) ** n2)
        "%" -> realMod n1 n2
        o -> error $ "Unimplemented operator " ++ o
    where realMod a b = a - fromInteger (floor $ a/b) * b
