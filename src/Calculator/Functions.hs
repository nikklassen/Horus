module Calculator.Functions (
    Function(..),
    buildFunction,

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
                         } deriving (Show, Eq)

buildFunction :: [AST] -> AST -> Function
buildFunction parameters = Function (getNames parameters)

getNames :: [AST] -> [String]
getNames (Var name : vs) = name : getNames vs
getNames [] = []
getNames ast = error $ "Unexpected \"" ++ show ast ++ "\" in parameter list"

isFunction :: String -> Bool
isFunction f = Map.member f functions

getFunction :: String -> Maybe ([CReal] -> CReal)
getFunction f = Map.lookup f functions

functions :: Map String ([CReal] -> CReal)
functions = Map.fromList $ map (second applyToFirst)
                [ ("sin", sin)
                , ("cos", cos)
                , ("tan", tan)
                , ("asin", asin)
                , ("acos", acos)
                , ("atan", atan)
                , ("sqrt", sqrt)
                , ("ln", log)
                , ("log", logBase 10)
                ]

applyToFirst :: (a -> a) -> [a] -> a
applyToFirst f (x:[]) = f x
applyToFirst _ _ = error "Unexpected number of arguments"
