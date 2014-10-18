{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.SynCheck.Tests (
    tests
) where

import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.Functions
import Calculator.DeepSeq()
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
        , testCase "No op" noop
        ]

emptyEnv = Env Map.empty Map.empty

dupParam = deepAssertRaises "Duplicate parameter a" $ synCheckPass (EqlStmt (FuncExpr "f" [Var "a", Var "a"]) (Number 1)) emptyEnv

nonVarParam = deepAssertRaises "Unexpected parameter 2.0" $ synCheckPass (EqlStmt (FuncExpr "f" [Var "a", Number 2]) (Number 1)) emptyEnv

undefParam = deepAssertRaises "Use of undefined parameter b" $ synCheckPass (EqlStmt (FuncExpr "f" [Var "a"]) (OpExpr "+" (Number 2) (Var "b"))) emptyEnv

useParam = synCheckPass eqlStmt emptyEnv @?== eqlStmt
           where eqlStmt = EqlStmt (FuncExpr "f" [Var "a"]) (Var "a")

useGlobal = synCheckPass eqlStmt (Env (Map.fromList [("a", Number 2)]) Map.empty) @?== eqlStmt
            where eqlStmt = EqlStmt (FuncExpr "f" []) (Var "a")

recursiveBind = deepAssertRaises "Recursive use of bound variable a" (synCheckPass stmt env)
                where stmt = BindStmt (Var "a") (OpExpr "+" (Var "b") (Number 2))
                      env = Env (Map.fromList [("b", Var "a")]) Map.empty

recursiveFunc = deepAssertRaises "Recursive use of function f" (synCheckPass stmt env)
                where stmt = EqlStmt (FuncExpr "f" [Var "a"]) (FuncExpr "m" [Var "a"])
                      env = Env Map.empty $ Map.fromList [("m", Function ["a"] (FuncExpr "f" [Var "a"]))]

noop = synCheckPass expr emptyEnv @?== expr
       where expr = OpExpr "+" (Number 2) (Number 2)
