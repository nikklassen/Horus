{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.ASTPass.SynCheck.Tests (
    tests
) where

import Calculator.ASTPass.SynCheck
import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.Data.Function
import Calculator.Parser
import Test.Framework.Providers.HUnit
import TestHelpers
import qualified Data.Map as Map (fromList, empty)

tests = [ testCase "Duplicate parameter" dupParam
        , testCase "Non-var parameter" nonVarParam
        , testCase "Undefined parameter" undefParam
        , testCase "Use of parameter" useParam
        , testCase "Use of global var" useGlobal
        , testCase "Recursive definition of bound var" recursiveBind
        , testCase "Non-recursive bound variable" nonRecursiveBind
        , testCase "Recursive function definition" recursiveFunc
        , testCase "Non-recursive function definition" nonRecursiveFunc
        , testCase "Assigment to number" assignNum
        , testCase "Bind to number" bindNum
        , testCase "No op" noop
        ]

emptyEnv = Env Map.empty Map.empty

dupParam = deepAssertRaises "Duplicate parameter a" $ synCheckPass emptyEnv [m|f(a, a) = 1|]

nonVarParam = deepAssertRaises "Unexpected parameter 2.0" $ synCheckPass emptyEnv [m|f(a, 2) = 1|]

undefParam = deepAssertRaises "Use of undefined parameter b" $ synCheckPass emptyEnv [m|f(a) = 2 + b|]

useParam = synCheckPass emptyEnv eqlStmt @?== eqlStmt
           where eqlStmt = [m|f(a) = a|]

useGlobal = synCheckPass (Env (Map.fromList [("a", Number 2)]) Map.empty) eqlStmt @?== eqlStmt
            where eqlStmt = [m|f() = a|]

recursiveBind = deepAssertRaises "Recursive use of bound variable a" (synCheckPass env stmt)
                where stmt = [m|a := b + 2|]
                      env = Env (Map.fromList [("b", Var "a")]) Map.empty

nonRecursiveBind = synCheckPass env stmt @?== stmt
                   where stmt = [m|a := b + 2|]
                         env = Env (Map.fromList [("b", Number 2)]) Map.empty

recursiveFunc = deepAssertRaises "Recursive use of function f" (synCheckPass env stmt)
                where stmt = [m|f(a) = m(a)|]
                      env = Env Map.empty $ Map.fromList [("m", Function ["a"] [m|f(a)|])]

nonRecursiveFunc = synCheckPass env stmt @?== stmt
                   where stmt = [m|f(a) = m(a)|]
                         env = Env Map.empty $ Map.fromList [("m", Function ["a"] [m|a + 4|])]

assignNum = deepAssertRaises "Cannot assign value to 2.0" $ synCheckPass emptyEnv (EqlStmt (Number 2) (Number 3))

bindNum = deepAssertRaises "Cannot bind value to 2.0" $ synCheckPass emptyEnv (BindStmt (Number 2) (Number 3))

noop = synCheckPass emptyEnv expr @?== expr
       where expr = [m|2 + 2|]
