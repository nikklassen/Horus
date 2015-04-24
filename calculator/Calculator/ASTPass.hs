module Calculator.ASTPass (
    runASTPasses
) where

import Calculator.ASTPass.ConstExpansion
import Calculator.ASTPass.SynCheck
import Calculator.Data.AST
import Calculator.Data.Env

runASTPasses :: Env -> AST -> AST
runASTPasses env ast = foldl (\astAcc pass -> pass astAcc) ast passes
                   where passes = [ constExpansionPass
                                  , synCheckPass env
                                  ]
