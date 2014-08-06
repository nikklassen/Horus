module Calculator.Canon (
    canonPass
) where

import Calculator.Canon.Internal
import Calculator.Data.AST
import Calculator.VarCheck

-- Only run folding on function assignment
canonPass :: AST -> AST
canonPass ast@(EqlStmt _ _) = varCheckPass $ canon ast
canonPass ast = ast
