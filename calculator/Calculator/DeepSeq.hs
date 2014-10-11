{-# OPTIONS_GHC -fno-warn-orphans #-}

module Calculator.DeepSeq where

import Calculator.Data.AST
import Calculator.Data.Result
import Calculator.Functions
import Control.DeepSeq
import Control.DeepSeq.Generics
import Data.Number.CReal

instance NFData CReal

instance NFData Result where
    rnf (Result r v f b) = rnf r `seq` rnf v `seq` rnf f `seq` rnf b

instance NFData Function where
    rnf (Function p b) = rnf p `seq` rnf b

instance NFData AST where
    rnf = genericRnf
