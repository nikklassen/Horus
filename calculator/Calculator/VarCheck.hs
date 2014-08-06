module Calculator.VarCheck (
    varCheckPass
) where

import Calculator.Data.AST
import Calculator.VarCheck.Internal

varCheckPass :: AST -> AST
varCheckPass ast@(EqlStmt _ _) = varCheck ast
varCheckPass ast = ast
