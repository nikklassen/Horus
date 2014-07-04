{-# OPTIONS_GHC -fno-warn-orphans #-}

module Calculator.DeepSeq where

import Calculator
import Calculator.Functions
import Calculator.Data.AST

import Data.Number.CReal
import Control.DeepSeq
import Control.DeepSeq.Generics

instance NFData CReal

instance NFData Result where
    rnf (Result r v f) = rnf r `seq` rnf v `seq` rnf f

instance NFData Function where
    rnf (Function p b) = rnf p `seq` rnf b

instance NFData AST where
    rnf = genericRnf
