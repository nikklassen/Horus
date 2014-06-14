{-# LANGUAGE DeriveGeneric #-}

module Calculator.Data.AST (
    AST(..)
) where

import GHC.Generics

data AST = EqlStmt AST AST
           | OpExpr String AST AST
           | FuncExpr String [AST]
           | Var String
           | Number String
           | Neg AST
           deriving (Eq, Generic)

showArgs :: [AST] -> String
showArgs [] = ""
showArgs (a:[]) = show a
showArgs (a:as) = show a ++ ", " ++ showArgs as

instance Show AST where
    show (EqlStmt lhs rhs) = show lhs ++ " = " ++ show rhs

    show (OpExpr op lhs rhs) = "(" ++ show lhs ++ " " ++ op ++ " " ++ show rhs ++ ")"

    show (FuncExpr name args) = name ++ "(" ++ showArgs args ++ ")"

    show (Var v) = v

    show (Number n) = n

    show (Neg e) = "-" ++ show e
