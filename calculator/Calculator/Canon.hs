module Calculator.Canon (
    canonPass
) where

import Calculator.Data.AST
import Calculator.Canon.Internal

-- Only run folding on function assignment
canonPass :: AST -> AST
canonPass ast@(EqlStmt _ _) = canon ast
canonPass ast = ast
