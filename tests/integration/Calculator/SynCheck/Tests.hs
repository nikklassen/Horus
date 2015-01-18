{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.SynCheck.Tests (
    tests
) where

import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.Data.Function
import Calculator.Parser
import Calculator.SynCheck
import Test.Framework.Providers.HUnit
import TestHelpers
import qualified Data.Map as Map (fromList, empty)

tests = [ testCase "Duplicate parameter" dupParam
        , testCase "Non-var parameter" nonVarParam
        , testCase "Undefined parameter" undefParam
        , testCase "Use of parameter" useParam
        , testCase "Use of global var" useGlobal
        , testCase "Recursive definition of bound var" recursiveBind
        , testCase "Recursive function definition" recursiveFunc
        , testCase "Assigment to reserved var name" reserved
        , testCase "No op" noop
        ]

emptyEnv = Env Map.empty Map.empty

dupParam = deepAssertRaises "Duplicate parameter a" $ synCheckPass [m|f(a, a) = 1|] emptyEnv

nonVarParam = deepAssertRaises "Unexpected parameter 2.0" $ synCheckPass [m|f(a, 2) = 1|] emptyEnv

undefParam = deepAssertRaises "Use of undefined parameter b" $ synCheckPass [m|f(a) = 2 + b|] emptyEnv

useParam = synCheckPass eqlStmt emptyEnv @?== eqlStmt
           where eqlStmt = [m|f(a) = a|]

useGlobal = synCheckPass eqlStmt (Env (Map.fromList [("a", Number 2)]) Map.empty) @?== eqlStmt
            where eqlStmt = [m|f() = a|]

recursiveBind = deepAssertRaises "Recursive use of bound variable a" (synCheckPass stmt env)
                where stmt = [m|a := b + 2|]
                      env = Env (Map.fromList [("b", Var "a")]) Map.empty

recursiveFunc = deepAssertRaises "Recursive use of function f" (synCheckPass stmt env)
                where stmt = [m|f(a) = m(a)|]
                      env = Env Map.empty $ Map.fromList [("m", Function ["a"] (FuncExpr "f" [Var "a"]))]

reserved = deepAssertRaises "Cannot assign to reserved variable name e" $ synCheckPass [m|e = 3|] emptyEnv

noop = synCheckPass expr emptyEnv @?== expr
       where expr = [m|2 + 2|]
