module Calculator.SynCheck (
    synCheckPass
) where

import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.SynCheck.Internal

synCheckPass :: AST -> Env -> AST
synCheckPass = synCheck
