{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Serializer where

import Calculator.Data.Env
import Calculator.Data.Decimal
import Calculator.Data.Function
import Data.Text
import Data.Aeson

instance ToJSON Decimal where
    toJSON = toValue

instance ToJSON Function where
    toJSON = toValue

instance ToJSON UserPrefs where
    toJSON UserPrefs{..} = object [ "isRadians" .= isRadians ]

toValue :: Show a => a -> Value
toValue = String . pack . show
