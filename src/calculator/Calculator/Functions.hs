module Calculator.Functions (
    isFunction,
    getFunction
) where

import Calculator.Data.Decimal
import Calculator.Data.Env (UserPrefs(..))
import Calculator.Error
import Control.Arrow (second)
import Data.Map (Map)
import qualified Data.Map as Map

isFunction :: String -> UserPrefs -> Bool
isFunction f = Map.member f . functions

getFunction :: String -> UserPrefs -> Maybe ([Decimal] -> Safe Decimal)
getFunction f = Map.lookup f . functions

functions :: UserPrefs -> Map String ([Decimal] -> Safe Decimal)
functions prefs = Map.fromList $
                    -- Multi argument functions
                    [ ("root", root)
                    , ("log", log')
                    ] ++
                    map (second applyToFirst)
                    (map (second (return .))
                            -- Power
                            [ ("sqrt", sqrt)
                            , ("exp", exp)

                            -- Integer
                            , ("ceil", fromIntegral' . ceiling)
                            , ("floor", fromIntegral' . floor)
                            , ("round", fromIntegral' . round)

                            -- Other
                            , ("ln", log)
                            ] ++
                     [ ("!", fact)
                     , ("fact", fact)
                     ] ++
                     -- Trig
                     map (\(name, f) -> (name, return . f . trigConvert))
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

fact :: Decimal -> Safe Decimal
fact n = if n == fromIntegral' (round n) && n >= 0 then
             return $ product [1..n]
         else
             throwError "Factorial can only be applied to non-negative integers"

root :: [Decimal] -> Safe Decimal
root [n, x] = return $ x**(1/n)
root _ = throwError "Unexpected number of arguments"

log' :: [Decimal] -> Safe Decimal
log' [b, x] = return $ logBase b x
log' [x] = return $ logBase 10 x
log' _ = throwError "Unexpected number of arguments"

fromIntegral' :: Integer -> Decimal
fromIntegral' = fromIntegral

applyToFirst :: (a -> Safe a) -> [a] -> Safe a
applyToFirst f [x] = f x
applyToFirst _ _ = throwError "Unexpected number of arguments"
