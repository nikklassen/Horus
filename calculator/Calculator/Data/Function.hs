module Calculator.Data.Function (
    Function(..),
    buildFunction,
    showDeclaration
) where

import Calculator.Data.AST
import Control.DeepSeq
import Data.SafeCopy

data Function = Function { params :: [String]
                         , body :: AST
                         } deriving (Eq)

instance NFData Function where
    rnf (Function p b) = rnf p `seq` rnf b

instance Show Function where
    show f@(Function _ b) = showDeclaration f ++ "= " ++ show b

showDeclaration :: Function -> String
showDeclaration (Function p _) = "(" ++ showArgs p ++ ")"

showArgs :: [String] -> String
showArgs [] = ""
showArgs [a] = a
showArgs (a:as) = a ++ ", " ++ showArgs as

buildFunction :: [AST] -> AST -> Function
buildFunction parameters = Function (getNames parameters)

getNames :: [AST] -> [String]
getNames (Var name : vs) = name : getNames vs
getNames [] = []
getNames (ast:_) = error $ "Unexpected expression \"" ++ show ast ++ "\" in parameter list"

deriveSafeCopy 0 'base ''Function
