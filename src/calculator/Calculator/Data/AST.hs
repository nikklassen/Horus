{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Calculator.Data.AST (
    AST(..),
    astMap
) where

import Calculator.Data.Decimal
import Calculator.Error
import Data.Data
import Data.SafeCopy
import GHC.Generics

data AST = EqlStmt AST AST
           | BindStmt AST AST
           | OpExpr String AST AST
           | FuncExpr String [AST]
           | Var String
           | Number Decimal
           | Neg AST

           -- Antiquotation
           | AVar String
           | ANum String
           | AId String
           deriving (Eq, Generic, Typeable, Data)

astMap :: (AST -> Safe AST) -> AST -> Safe AST
astMap f (EqlStmt lhs rhs) = do
    lhs' <- astMap f lhs
    rhs' <- astMap f rhs
    f $ EqlStmt lhs' rhs'
astMap f (BindStmt lhs rhs) = do
    lhs' <- astMap f lhs
    rhs' <- astMap f rhs
    f $ BindStmt lhs' rhs'

astMap f (OpExpr op lhs rhs) = do
    lhs' <- astMap f lhs
    rhs' <- astMap f rhs
    f $ OpExpr op lhs' rhs'

astMap f (FuncExpr name ps) = mapM (astMap f) ps >>= f . FuncExpr name
astMap f v@(Var _) = f v
astMap f n@(Number _) = f n
astMap f (Neg a) = astMap f a >>= f . Neg
astMap _ a = throwError $ "Invalid ast " ++ show a

showArgs :: [AST] -> String
showArgs [] = ""
showArgs (a:[]) = show a
showArgs (a:as) = show a ++ ", " ++ showArgs as

instance Show AST where
    show (EqlStmt lhs rhs) = show lhs ++ " = " ++ show rhs

    show (BindStmt lhs rhs) = show lhs ++ " := " ++ show rhs

    show (OpExpr op lhs rhs) = "(" ++ show lhs ++ " " ++ op ++ " " ++ show rhs ++ ")"

    show (FuncExpr name args) = name ++ "(" ++ showArgs args ++ ")"

    show (Var v) = v

    show (Number n) = show n

    show (Neg e) = "-" ++ show e

    show (AVar v) = "$v:" ++ v

    show (ANum n) = "$n:" ++ n

    show (AId i) = '$' : i

deriveSafeCopy 0 'base ''AST
