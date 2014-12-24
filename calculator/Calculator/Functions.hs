module Calculator.Functions (
    isFunction,
    getFunction
) where

import Calculator.Data.Decimal
import Calculator.Data.Env (UserPrefs(..))
import Control.Arrow (second)
import Data.Map (Map)
import qualified Data.Map as Map

isFunction :: String -> UserPrefs -> Bool
isFunction f = Map.member f . functions

getFunction :: String -> UserPrefs -> (Maybe ([Decimal] -> Decimal))
getFunction f = Map.lookup f . functions

functions :: UserPrefs -> (Map String ([Decimal] -> Decimal))
functions prefs = Map.fromList $
                    -- Multi argument functions
                    [ ("root", root)
                    , ("log", log')
                    ] ++
                    map (second applyToFirst)
                    -- Power
                    ([ ("sqrt", sqrt)
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
                     -- Trig
                     map (second (. trigConvert))
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
                         ])
                 -- All built-in trig functions take their input in radians
                 where trigConvert = if isRadians prefs then id else (* (pi / 180))

fact :: Decimal -> Decimal
fact n = if n == fromIntegral' (round n) && n >= 0 then
             product [1..n]
         else
             error "Factorial can only be applied to non-negative integers"

root :: [Decimal] -> Decimal
root (n:x:[]) = x**(1/n)
root _ = error "Unexpected number of arguments"

log' :: [Decimal] -> Decimal
log' (b:x:[]) = logBase b x
log' (x:[]) = logBase 10 x
log' _ = error "Unexpected number of arguments"

fromIntegral' :: Integer -> Decimal
fromIntegral' = fromIntegral

applyToFirst :: (a -> a) -> [a] -> a
applyToFirst f (x:[]) = f x
applyToFirst _ _ = error "Unexpected number of arguments"
