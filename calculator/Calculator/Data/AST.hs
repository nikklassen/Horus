{-# LANGUAGE DeriveGeneric #-}

module Calculator.Data.AST (
    AST(..),
    astMap
) where

import Data.Number.CReal
import GHC.Generics

data AST = EqlStmt AST AST
           | OpExpr String AST AST
           | FuncExpr String [AST]
           | Var String
           | Number CReal
           | Neg AST
           deriving (Eq, Generic)

astMap :: (AST -> AST) -> AST -> AST
astMap f (EqlStmt lhs rhs) = f $ EqlStmt (astMap f lhs) (astMap f rhs)
astMap f (OpExpr op lhs rhs) = f $ OpExpr op (astMap f lhs) (astMap f rhs)
astMap f (FuncExpr name ps) = f $ FuncExpr name (map (astMap f) ps)
astMap f v@(Var _) = f v
astMap f n@(Number _) = f n
astMap f (Neg a) = f $ Neg (astMap f a)

showArgs :: [AST] -> String
showArgs [] = ""
showArgs (a:[]) = show a
showArgs (a:as) = show a ++ ", " ++ showArgs as

instance Show AST where
    show (EqlStmt lhs rhs) = show lhs ++ " = " ++ show rhs

    show (OpExpr op lhs rhs) = "(" ++ show lhs ++ " " ++ op ++ " " ++ show rhs ++ ")"

    show (FuncExpr name args) = name ++ "(" ++ showArgs args ++ ")"

    show (Var v) = v

    show (Number n) = show n

    show (Neg e) = "-" ++ show e
