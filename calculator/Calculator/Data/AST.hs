{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TemplateHaskell #-}

module Calculator.Data.AST (
    AST(..),
    astMap
) where

import Calculator.Data.Decimal
import Control.DeepSeq.Generics
import Data.Data
import GHC.Generics
import Data.SafeCopy

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

instance NFData AST where
    rnf = genericRnf

astMap :: (AST -> AST) -> AST -> AST
astMap f (EqlStmt lhs rhs) = f $ EqlStmt (astMap f lhs) (astMap f rhs)
astMap f (BindStmt lhs rhs) = f $ BindStmt (astMap f lhs) (astMap f rhs)
astMap f (OpExpr op lhs rhs) = f $ OpExpr op (astMap f lhs) (astMap f rhs)
astMap f (FuncExpr name ps) = f $ FuncExpr name (map (astMap f) ps)
astMap f v@(Var _) = f v
astMap f n@(Number _) = f n
astMap f (Neg a) = f $ Neg (astMap f a)
astMap _ a = error $ "Invalid ast " ++ show a

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
