module Calculator.Canon (
    canonPass
) where

import Calculator.Canon.Internal
import Calculator.Data.AST

canonPass :: AST -> AST
canonPass = canon
