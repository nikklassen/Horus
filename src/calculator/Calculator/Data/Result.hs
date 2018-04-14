module Calculator.Data.Result (
    Result(..)
) where

import Calculator.Data.AST (AST)
import Calculator.Data.Decimal (Decimal)
import Calculator.Data.Function (Function)
import Data.Map (Map)

data Result = FuncResult {
                  name :: String,
                  def :: Function
              }
              | VarResult {
                  answer :: Decimal,
                  name :: String,
                  value :: AST,
                  boundResults :: Map String Decimal
              }
              | CalcResult {
                  answer :: Decimal
              } deriving (Eq, Show)
