module Calculator.ASTPass (
    runASTPasses
) where

import Calculator.ASTPass.ConstExpansion
import Calculator.ASTPass.SynCheck
import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.Error (Safe)
import Control.Monad (foldM)

runASTPasses :: Env -> AST -> Safe AST
runASTPasses env ast = foldM (\astAcc pass -> pass astAcc) ast passes
                   where passes = [ constExpansionPass
                                  , synCheckPass env
                                  ]
