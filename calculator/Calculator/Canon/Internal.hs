{-# LANGUAGE PatternSynonyms #-}

module Calculator.Canon.Internal (
    canon
) where

import Calculator.Data.AST
import Calculator.Data.Decimal
import Calculator.Error
import Calculator.Evaluator.Helpers
import Calculator.Parser

-- show defaults to 40 decimal places, if there are less than this the
-- answer is quaranteed to be exact.  To save on space, we want to make
-- sure all answers stored are less than 81 digits (before and after dec)
shouldFold :: Decimal -> Bool
shouldFold n = length (dropWhile (not . (== '.')) s) <= 40 && length s <= 81
               where s = show n

eval' :: String -> Decimal -> Decimal -> Safe AST
eval' op lhs rhs = do
    res <- operate op lhs rhs
    if shouldFold res then
        return $ Number res
        else
            return $ OpExpr op (Number lhs) (Number rhs)

pattern AddExpr lhs rhs = OpExpr "+" lhs rhs
pattern SubExpr lhs rhs = OpExpr "-" lhs rhs
pattern MultExpr lhs rhs = OpExpr "*" lhs rhs

-- Based on reduction rules from "Advanced Compiler Design and
-- Implementation" by Steven S. Muchnick

-- Canonical represenation
--  - All numbers in left subtree
--  - Sum of products
canon :: AST -> Safe AST
canon (MultExpr lhs rhs) = do
    lhsCanon <- canon lhs
    rhsCanon <- canon rhs
    case (lhsCanon, rhsCanon) of
        -- Reduce constants
        (Number a, Number b) -> eval' "*" a b

        -- Distribution
        -- Over addition
        (AddExpr (Number n1) mRhs, num@(Number n2)) -> do
            lhs' <- eval' "*" n1 n2
            return $ OpExpr "+" lhs' (OpExpr "*" num mRhs)
        (num@(Number n1), AddExpr (Number n2) mRhs) -> do
            lhs' <- eval' "*" n1 n2
            return $ OpExpr "+" lhs' (OpExpr "*" num mRhs)
        (n@(Number _), [m|$aLhs + $aRhs|]) -> return $ [m|($n * $aLhs) + ($n * $aRhs)|]
        ([m|$aLhs + $aRhs|], n@(Number _)) -> return $ [m|($n * $aLhs) + ($n * $aRhs)|]
        -- Over subtraction
        (n@(Number _), [m|$aLhs - $aRhs|]) -> return [m|($n * $aLhs) - ($n * $aRhs)|]
        ([m|$aLhs - $aRhs|], n@(Number _)) -> return [m|($n * $aLhs) - ($n * $aRhs)|]

        -- In general
        ([m|$aLhs + $aRhs|], ast) -> return [m|($aLhs * $ast) + ($aRhs * $ast)|]
        (ast, [m|$aLhs + $aRhs|]) -> return [m|($ast * $aLhs) + ($ast * $aRhs)|]
        ([m|$aLhs - $aRhs|], ast) -> return [m|($aLhs * $ast) - ($aRhs * $ast)|]
        (ast, [m|$aLhs - $aRhs|]) -> return [m|($ast * $aLhs) - ($ast * $aRhs)|]

        -- Move numbers left
        ([m|$n:n1 * $mRhs|], Number n2) -> do
            lhs' <- eval' "*" n1 n2
            return $ OpExpr "*" lhs' mRhs
        (Number n1, [m|$n:n2 * $mRhs|]) -> do
            lhs' <- eval' "*" n1 n2
            return $ OpExpr "*" lhs' mRhs
        (ast, n@(Number _)) -> return $ OpExpr "*" n ast

        -- Associate left
        (ast, [m|$mLhs * $mRhs|]) -> return [m|($ast * $mLhs) * $mRhs|]

        (mLhs, mRhs) -> return [m|$mLhs * $mRhs|]

canon (AddExpr lhs rhs) = do
    lhsCanon <- canon lhs
    rhsCanon <- canon rhs
    case (lhsCanon, rhsCanon) of
        -- Reduce constants
        (Number a, Number b) -> eval' "+" a b

        -- Move numbers left
        (AddExpr (Number n1) aRhs, Number n2) -> do
            lhs' <- eval' "+" n1 n2
            return $ OpExpr "+" lhs' aRhs
        (Number n1, AddExpr (Number n2) aRhs) -> do
            lhs' <- eval' "+" n1 n2
            return $ OpExpr "+" lhs' aRhs
        (ast, n@(Number _)) -> return $ OpExpr "+" n ast

        -- Assosiate left
        (ast, [m|$aLhs + $aRhs|]) -> return [m|($ast + $aLhs) + $aRhs|]

        (aLhs, aRhs) -> return [m|$aLhs + $aRhs|]

-- Remove subtraction
canon (SubExpr lhs rhs) = do
    lhsCanon <- canon lhs
    rhsCanon <- canon rhs
    case (lhsCanon, rhsCanon) of
        -- Reduce constants
        (Number a, Number b) -> eval' "-" a b

        -- Move numbers left
        (ast, Number n) -> return $ OpExpr "+" (Number $ negate n) ast

        (aLhs, aRhs) -> return [m|$aLhs - $aRhs|]

-- Any other operations
canon (OpExpr op lhs rhs) = do
    lhsCanon <- canon lhs
    rhsCanon <- canon rhs
    case (lhsCanon, rhsCanon) of
        (Number n1, Number n2) -> eval' op n1 n2
        (newLhs, newRhs) -> return $ OpExpr op newLhs newRhs

canon (EqlStmt lhs rhs) = EqlStmt lhs <$> canon rhs

canon (BindStmt lhs rhs) = BindStmt lhs <$> canon rhs

canon (FuncExpr func args) = FuncExpr func <$> mapM canon args

canon (Neg e) = do
    c <- canon e
    return $ case c of
        (Number n) -> Number $ negate n
        a -> Neg a

-- Exhaustive matches (no ops)
canon v@(Var _) = return v
canon n@(Number _) = return n

canon ast = throwError $ "Invalid expression \"" ++ show ast ++ "\""
