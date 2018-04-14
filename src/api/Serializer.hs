{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Serializer where

import Calculator.Data.AST
import Calculator.Data.Decimal
import Calculator.Data.Env
import Calculator.Data.Function
import Data.Aeson hiding (Number)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Types (Pair)
import Data.Map (Map, (!))
import Data.Text
import Happstack.Server (Response, addHeader, toResponse)

instance ToJSON Decimal where
    toJSON = toValue

instance ToJSON Function where
    toJSON = toValue

toValue :: Show a => a -> Value
toValue = String . pack . show

varsToJSON :: Map String Decimal -> String -> (AST, String) -> Value
varsToJSON _ _ (Number n, _) = object [ "value" .= n ]
varsToJSON bound v (_, text) =  object [ "value" .= (bound ! v)
                                       , "expr" .= text
                                       ]

funcsToJSON :: String -> (Function, String) -> Value
funcsToJSON k (f, text) = object [ "decl" .= (k ++ showDeclaration f)
                                 , "def" .= text
                                 ]

jsonResponse :: [Pair] -> Response
jsonResponse = addHeader "Content-Type" "application/json" . toResponse . encode . object

$(deriveJSON defaultOptions ''UserPrefs)
