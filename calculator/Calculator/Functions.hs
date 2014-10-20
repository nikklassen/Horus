module Calculator.Functions (
    Function(..),
    buildFunction,
    showDeclaration,

    isFunction,
    getFunction
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Number.CReal
import Control.Arrow (second)
import Calculator.Data.AST

data Function = Function { params :: [String]
                         , body :: AST
                         } deriving (Eq)

showArgs :: [String] -> String
showArgs [] = ""
showArgs (a:[]) = a
showArgs (a:as) = a ++ ", " ++ showArgs as

instance Show Function where
    show f@(Function _ b) = showDeclaration f ++ "= " ++ show b

showDeclaration :: Function -> String
showDeclaration (Function p _) = "(" ++ showArgs p ++ ")"

buildFunction :: [AST] -> AST -> Function
buildFunction parameters = Function (getNames parameters)

getNames :: [AST] -> [String]
getNames (Var name : vs) = name : getNames vs
getNames [] = []
getNames (ast:_) = error $ "Unexpected expression \"" ++ show ast ++ "\" in parameter list"

isFunction :: String -> Bool
isFunction f = Map.member f functions

getFunction :: String -> Maybe ([CReal] -> CReal)
getFunction f = Map.lookup f functions

functions :: Map String ([CReal] -> CReal)
functions = Map.fromList $ map (second applyToFirst)
                -- Trig
                [ ("sin", sin)
                , ("cos", cos)
                , ("tan", tan)
                , ("asin", asin)
                , ("acos", acos)
                , ("atan", atan)
                , ("sinh", sinh)
                , ("cosh", cosh)
                , ("tanh", tanh)
                , ("asinh", asinh)
                , ("acosh", acosh)
                , ("atanh", atanh)
                , ("deg", deg)

                -- Power
                , ("sqrt", sqrt)
                , ("exp", exp)

                -- Integer
                , ("ceil", fromIntegral' . ceiling)
                , ("floor", fromIntegral' . floor)
                , ("round", fromIntegral' . round)

                -- Other
                , ("!", fact)
                , ("fact", fact)
                , ("ln", log)
                ] ++

                -- Multi argument functions
                [ ("root", root)
                , ("log", log')
                ]

deg :: CReal -> CReal
deg = (*) (pi / 180)

fact :: CReal -> CReal
fact n = if n == fromIntegral' (round n) && n >= 0 then
             product [1..n]
         else
             error "Factorial can only be applied to non-negative integers"

root :: [CReal] -> CReal
root (n:x:[]) = x**(1/n)
root _ = error "Unexpected number of arguments"

log' :: [CReal] -> CReal
log' (b:x:[]) = logBase b x
log' (x:[]) = logBase 10 x
log' _ = error "Unexpected number of arguments"

fromIntegral' :: Integer -> CReal
fromIntegral' = fromIntegral

applyToFirst :: (a -> a) -> [a] -> a
applyToFirst f (x:[]) = f x
applyToFirst _ _ = error "Unexpected number of arguments"
