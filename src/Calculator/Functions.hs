module Calculator.Functions (
    isFunction,
    getFunction
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Number.CReal

functions :: Map String (CReal -> CReal)
functions = Map.fromList [ ("sin", sin)
                         , ("asin", asin)
                         , ("cos", cos)
                         , ("acos", acos)
                         , ("tan", tan)
                         , ("atan", atan)
                         , ("sqrt", sqrt)
                         , ("ln", ln)
                         , ("log", log)
                         ]

isFunction :: String -> Bool
isFunction f = Map.member f functions

getFunction :: String -> Maybe (CReal -> CReal)
getFunction f = Map.lookup f functions

ln :: CReal -> CReal
ln = logBase (exp 1)
