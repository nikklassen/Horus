{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.VarCheck.Tests (
    tests
) where

import Calculator.Data.AST
import Calculator.DeepSeq()
import Calculator.VarCheck.Internal
import Control.Exception (ErrorCall(..), evaluate)
import qualified Control.Exception.Lifted as CEL
import Control.DeepSeq (($!!))
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Tools

tests = [ testCase "Duplicate parameter" dupParam
        , testCase "Non-var parameter" nonVarParam
        , testCase "Undefined parameter" undefParam
        , testCase "Valid func def" validFuncDef
        ]

dupParam = assertRaises "" (ErrorCall "Duplicate parameter a")
                           (CEL.evaluate $!! varCheck $ EqlStmt (FuncExpr "f" [Var "a", Var "a"]) (Number 1))

nonVarParam = assertRaises "" (ErrorCall "Unexpected parameter 2.0")
                              (CEL.evaluate $!! varCheck $ EqlStmt (FuncExpr "f" [Var "a", Number 2]) (Number 1))

undefParam = assertRaises "" (ErrorCall "Use of undefined parameter b")
                             (CEL.evaluate $!! varCheck $ EqlStmt (FuncExpr "f" [Var "a"]) (Var "b"))

validFuncDef = varCheck eqlStmt @?= eqlStmt
               where eqlStmt = EqlStmt (FuncExpr "f" [Var "a"]) (Var "a")
