{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Serializer where

import Calculator.Data.Env
import Calculator.Data.Decimal
import Calculator.Data.Function
import Data.Text
import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions)

instance ToJSON Decimal where
    toJSON = toValue

instance ToJSON Function where
    toJSON = toValue

toValue :: Show a => a -> Value
toValue = String . pack . show

$(deriveJSON defaultOptions ''UserPrefs)
