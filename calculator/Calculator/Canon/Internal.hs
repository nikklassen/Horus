{-# LANGUAGE PatternSynonyms #-}

module Calculator.Canon.Internal (
    canon
) where

import Calculator.Data.AST
import Calculator.Evaluator.Helpers
import Data.Number.CReal

-- show defaults to 40 decimal places, if there are less than this the
-- answer is quaranteed to be exact.  To save on space, we want to make
-- sure all answers stored are less than 81 digits (before and after dec)
shouldFold :: CReal -> Bool
shouldFold n = length (dropWhile (not . (== '.')) s) <= 40 && length s <= 81
               where s = show n

eval' :: String -> CReal -> CReal -> AST
eval' op lhs rhs = let res = operate op lhs rhs
                   in if shouldFold res then
                          Number res
                          else
                              OpExpr op (Number lhs) (Number rhs)

pattern AddExpr lhs rhs = OpExpr "+" lhs rhs
pattern SubExpr lhs rhs = OpExpr "-" lhs rhs
pattern MultExpr lhs rhs = OpExpr "*" lhs rhs

-- Based on reduction rules from "Advanced Compiler Design and
-- Implementation" by Steven S. Muchnick

-- Canonical represenation
--  - All numbers in left subtree
--  - Sum of products
canon :: AST -> AST
canon (MultExpr lhs rhs) = case (canon lhs, canon rhs) of
                               -- Reduce constants
                               (Number a, Number b) -> eval' "*" a b

                               -- Distribution
                               -- Over addition
                               (AddExpr (Number n1) mRhs, num@(Number n2)) -> OpExpr "+" (eval' "*" n1 n2) (OpExpr "*" num mRhs)
                               (num@(Number n1), AddExpr (Number n2) mRhs) -> OpExpr "+" (eval' "*" n1 n2) (OpExpr "*" num mRhs)
                               (n@(Number _), AddExpr aLhs aRhs) -> OpExpr "+" (OpExpr "*" n aLhs) (OpExpr "*" n aRhs)
                               (AddExpr aLhs aRhs, n@(Number _)) -> OpExpr "+" (OpExpr "*" n aLhs) (OpExpr "*" n aRhs)
                               -- Over subtraction
                               (n@(Number _), SubExpr aLhs aRhs) -> OpExpr "-" (OpExpr "*" n aLhs) (OpExpr "*" n aRhs)
                               (SubExpr aLhs aRhs, n@(Number _)) -> OpExpr "-" (OpExpr "*" n aLhs) (OpExpr "*" n aRhs)
                               -- In general
                               (AddExpr aLhs aRhs, ast) -> OpExpr "+" (OpExpr "*" aLhs ast) (OpExpr "*" aRhs ast)
                               (ast, AddExpr aLhs aRhs) -> OpExpr "+" (OpExpr "*" ast aLhs) (OpExpr "*" ast aRhs)
                               (SubExpr aLhs aRhs, ast) -> OpExpr "-" (OpExpr "*" aLhs ast) (OpExpr "*" aRhs ast)
                               (ast, SubExpr aLhs aRhs) -> OpExpr "-" (OpExpr "*" ast aLhs) (OpExpr "*" ast aRhs)

                               -- Move numbers left
                               (MultExpr (Number n1) mRhs, Number n2) -> OpExpr "*" (eval' "*" n1 n2) mRhs
                               (Number n1, MultExpr (Number n2) mRhs) -> OpExpr "*" (eval' "*" n1 n2) mRhs
                               (ast, n@(Number _)) -> OpExpr "*" n ast

                               -- Associate left
                               (ast, MultExpr mLhs mRhs) -> OpExpr "*" (OpExpr "*" ast mLhs) mRhs

                               (mLhs, mRhs) -> OpExpr "*" mLhs mRhs

canon (AddExpr lhs rhs) = case (canon lhs, canon rhs) of
                              -- Reduce constants
                              (Number a, Number b) -> eval' "+" a b

                              -- Move numbers left
                              (AddExpr (Number n1) aRhs, Number n2) -> OpExpr "+" (eval' "+" n1 n2) aRhs
                              (Number n1, AddExpr (Number n2) aRhs) -> OpExpr "+" (eval' "+" n1 n2) aRhs
                              (ast, n@(Number _)) -> OpExpr "+" n ast

                              -- Assosiate left
                              (ast, AddExpr aLhs aRhs) -> OpExpr "+" (OpExpr "+" ast aLhs) aRhs

                              (aRhs, aLhs) -> OpExpr "+" aRhs aLhs

-- Remove subtraction
canon (SubExpr lhs rhs) = case (canon lhs, canon rhs) of
                              -- Reduce constants
                              (Number a, Number b) -> eval' "-" a b

                              -- Move numbers left
                              (ast, Number n) -> OpExpr "+" (Number $ negate n) ast

                              (aRhs, aLhs) -> OpExpr "-" aRhs aLhs

-- Any other operations
canon (OpExpr op lhs rhs) = case (canon lhs, canon rhs) of
                                (Number n1, Number n2) -> eval' op n1 n2
                                (newLhs, newRhs) -> OpExpr op newLhs newRhs

canon (EqlStmt lhs rhs) = EqlStmt lhs $ canon rhs

canon (BindStmt lhs rhs) = BindStmt lhs $ canon rhs

canon (FuncExpr func args) = let newArgs = map canon args
                             in FuncExpr func newArgs

canon (Neg e) = case canon e of
                   (Number n) -> Number $ negate n
                   a -> Neg a

-- Exhaustive matches (no ops)
canon v@(Var _) = v
canon n@(Number _) = n
