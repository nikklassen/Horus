module Serializer (
    serializeVars,
    serializeFuncs,
    serializeResult
) where

import Calculator
import Calculator.Functions
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Number.CReal
import Text.JSON.Generic (toJSON)
import Text.JSON.Types
import Data.Text.Lazy (unpack, pack, splitOn)

serializeVars :: Map String CReal -> JSValue
serializeVars variables = JSObject $ toJSObject $ Map.assocs $ Map.map (toJSON . show) variables

serializeFuncs :: Map String Function -> JSValue
serializeFuncs functions = JSObject $ toJSObject $ map split $ Map.assocs functions
                           where split :: (String, Function) -> (String, JSValue)
                                 split (k, v) = let [lhs, rhs] = splitOn (pack " = ") $ pack $ k ++ show v
                                                in (unpack lhs, toJSON $ unpack rhs)

serializeResult :: Either String Result -> [(String, JSValue)]
serializeResult (Left err) = [("error", toJSON err)]
serializeResult (Right result) = [("result", toJSON $ show $ answer result)]

