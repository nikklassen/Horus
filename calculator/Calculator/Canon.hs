module Calculator.Canon (
    canonPass
) where

import Calculator.Canon.Internal
import Calculator.Data.AST (AST)
import Calculator.Error (Safe)

canonPass :: AST -> Safe AST
canonPass = canon
