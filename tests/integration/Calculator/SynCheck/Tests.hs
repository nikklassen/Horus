{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.SynCheck.Tests (
    tests
) where

import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.DeepSeq()
import Calculator.SynCheck.Internal
import Control.DeepSeq (($!!))
import Control.Exception (ErrorCall(..))
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Tools
import qualified Control.Exception.Lifted as CEL
import qualified Data.Map as Map (fromList, empty)

tests = [ testCase "Duplicate parameter" dupParam
        , testCase "Non-var parameter" nonVarParam
        , testCase "Undefined parameter" undefParam
        , testCase "Valid func def" validFuncDef
        , testCase "Recursive definition of bound var" recursiveBind
        , testCase "No op" noop
        ]

emptyEnv = Env Map.empty Map.empty

dupParam = assertRaises "" (ErrorCall "Duplicate parameter a")
                           (CEL.evaluate $!! synCheck (EqlStmt (FuncExpr "f" [Var "a", Var "a"]) (Number 1)) emptyEnv)

nonVarParam = assertRaises "" (ErrorCall "Unexpected parameter 2.0")
                              (CEL.evaluate $!! synCheck (EqlStmt (FuncExpr "f" [Var "a", Number 2]) (Number 1)) emptyEnv)

undefParam = assertRaises "" (ErrorCall "Use of undefined parameter b")
                             (CEL.evaluate $!! synCheck (EqlStmt (FuncExpr "f" [Var "a"]) (Var "b")) emptyEnv)

validFuncDef = synCheck eqlStmt emptyEnv @?= eqlStmt
               where eqlStmt = EqlStmt (FuncExpr "f" [Var "a"]) (Var "a")

recursiveBind = assertRaises "" (ErrorCall "Recursive use of bound variable a")
                                (CEL.evaluate $!! synCheck stmt env)
                                where stmt = BindStmt (Var "a") (OpExpr "+" (Var "b") (Number 2))
                                      env = Env (Map.fromList [("b", Var "a")]) Map.empty

noop = synCheck expr emptyEnv @?= expr
       where expr = OpExpr "+" (Number 2) (Number 2)
