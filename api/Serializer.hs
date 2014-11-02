{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Serializer where

import Calculator.Functions
import Calculator.Data.Decimal
import Data.Text
import Data.Aeson

instance ToJSON Decimal where
    toJSON = toValue

instance ToJSON Function where
    toJSON = toValue

toValue :: Show a => a -> Value
toValue = String . pack . show
