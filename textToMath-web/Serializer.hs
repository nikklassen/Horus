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
import Control.Arrow

serializeVars :: Map String CReal -> JSValue
serializeVars variables = JSObject $ toJSObject $ Map.assocs $ Map.map (toJSON . show) variables

serializeFuncs :: Map String Function -> JSValue
serializeFuncs functions = JSObject $ toJSObject $ map (second (toJSON . show)) $ Map.assocs functions

serializeResult :: Either String Result -> [(String, JSValue)]
serializeResult (Left err) = [("error", toJSON err)]
serializeResult (Right result) = [("result", toJSON $ show $ answer result)]

