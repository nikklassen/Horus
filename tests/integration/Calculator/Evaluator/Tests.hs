{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.Evaluator.Tests (
    tests
) where

import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.Evaluator
import Calculator.Data.Function
import Calculator.Parser
import Control.Exception (ErrorCall(..), evaluate)
import Data.Map (Map)
import Calculator.Data.Decimal
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Tools
import qualified Data.Map as Map

tests = [ testGroup "Simple"
            [ testCase "Int" int
            , testCase "Decimal" dec
            , testCase "Exponent" exponential
            , testCase "Decimal with exponent" decExp
            , testCase "Operator" op
            , testCase "Real mod" realMod
            , testCase "Negative number with power" negToPower
            , testCase "Negate" neg
            , testCase "Var" var
            , testCase "Assignment" eql
            , testCase "Bind" bind
            , testCase "Bind assignemnt" bindStmt
            , testCase "Pi" piTest
            ]
        , testGroup "Error"
            [ testCase "Undefined var" errorVar
            , testCase "Undefined function" errorFunc
            , testCase "Builtin function - wrong number of args" errorBuiltinFuncArgs
            , testCase "User function - wrong number of args" errorFuncArgs
            , testCase "Invalid AST" errorAST
            ]
        , testGroup "Functions"
            [ testCase "Function definition" functionDef
            , testCase "Function" function
            , testCase "User function" userFunction
            , testCase "Function in function" nestedFunction
            , testCase "Variable scoping" varScopes
            ]
        ]

process :: AST -> Decimal
process ast = fst $ evalPass ast defaultPrefs $ Env Map.empty Map.empty

processVars :: AST -> Map String AST -> (Decimal, Env)
processVars ast vars = evalPass ast defaultPrefs $ Env vars Map.empty

processFuncs :: AST -> Map String Function -> (Decimal, Env)
processFuncs ast funcs = evalPass ast defaultPrefs $ Env Map.empty funcs

int = process [m|0123456789876543210|] @?= 123456789876543210

dec = process [m|0.8|] @?= 0.8

exponential = process [m|9e-10|] @?= 9e-10

decExp = process [m|2.3e2|] @?= 230

op = process [m|1 + 1|] @?= 2

realMod = process [m|4 % 1.5|] @?= 1

negToPower = process [m|(-3) ^ (-2)|] @?= -1/9

neg = process [m|-2|] @?= -2

var = let vars = Map.fromList [("a", Number 4)]
      in processVars (Var "a") vars @?= (4, Env vars Map.empty)

eql = processVars [m|a = 4|] (Map.fromList [("a", Number 2)]) @?= (4, Env (Map.fromList [("a", Number 4)]) Map.empty)

bind = processVars [m|a + 2|] vars @?= (4, Env vars Map.empty)
       where vars = Map.fromList [("b", Number 2), ("a", Var "b")]

bindStmt = processVars [m|a := b|] vars @?= (2, Env vars Map.empty)
           where vars = Map.fromList [("b", Number 2), ("a", Var "b")]

piTest = process [m|pi * 3|] @?= pi * 3

errorVar = assertRaises "" (ErrorCall "Use of undefined variable \"a\"")
                           (evaluate $ process (Var "a"))

errorFunc = assertRaises "" (ErrorCall "Use of undefined function \"foo\"")
                            (evaluate $ process [m|foo(2)|])

errorBuiltinFuncArgs = assertRaises "" (ErrorCall "Unexpected number of arguments")
                                       (evaluate $ process [m|sin(2, 3)|])

errorFuncArgs = assertRaises "" (ErrorCall "Unexpected number of arguments")
                                -- Use some empty variables to force the evaluation of the expression
                                (processFuncs [m|foo(2, 3)|] fooFunc @?= (4, Env Map.empty Map.empty))
                                where fooFunc = Map.fromList [("foo", Function ["a"] (Var "a"))]

errorAST = assertRaises "" (ErrorCall $ "Cannot evaluate the statement " ++ show ast)
                           (evaluate $ process ast)
                           where ast = EqlStmt (Number 2) (Number 3)

functionDef = processFuncs [m|foo(b) = b + 2|] Map.empty @?= (0, Env Map.empty fooFunc)
              where fooFunc = Map.fromList [("foo", Function ["b"] [m|2 + b|])]

function = process [m|sin(1)|] @?= sin 1

userFunction = processFuncs [m|foo(2)|] fooFunc @?= (4, Env Map.empty fooFunc)
               where fooFunc = Map.fromList [("foo", Function ["b"] [m|b + 2|])]

nestedFunction = fst (evalPass [m|f(2)|] defaultPrefs env) @?= 2
                 where f = ("f", Function ["a"] [m|z(a)|])
                       z = ("z", Function ["a"] (Var "a"))
                       env = Env Map.empty $ Map.fromList [f, z]

-- The local x variable should be used when evaluating f
-- and the global x should be used when evaluating inner function m
varScopes = fst (evalPass [m|f(2)|] defaultPrefs env) @?= 13
            where f = ("f", Function ["x"] [m|x + z(3)|])
                  z = ("z", Function ["a"] [m|x + a|])
                  env = Env (Map.fromList [("x", Number 8)]) $ Map.fromList [f, z]
