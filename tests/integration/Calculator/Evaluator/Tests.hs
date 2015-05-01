{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.Evaluator.Tests (
    tests
) where

import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.Data.Function
import Calculator.Data.Result
import Calculator.Error (Safe)
import Calculator.Evaluator
import Calculator.Parser
import Data.Map (Map)
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import TestHelpers
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

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

process :: AST -> Safe Result
process ast = fst <$> evalPass ast defaultPrefs emptyEnv

processVars :: AST -> Map String AST -> Safe (Result, Env)
processVars ast vars = evalPass ast defaultPrefs $ Env vars Map.empty

processFuncs :: AST -> Map String Function -> Safe (Result, Env)
processFuncs ast funcs = evalPass ast defaultPrefs $ Env Map.empty funcs

int = process [m|0123456789876543210|] @?== CalcResult 123456789876543210

dec = process [m|0.8|] @?== CalcResult 0.8

exponential = process [m|9e-10|] @?== CalcResult 9e-10

decExp = process [m|2.3e2|] @?== CalcResult 230

op = process [m|1 + 1|] @?== CalcResult 2

realMod = process [m|4 % 1.5|] @?== CalcResult 1

negToPower = process [m|(-3) ^ (-2)|] @?== CalcResult (-1/9)

neg = process [m|-2|] @?== CalcResult (-2)

var = let vars = Map.fromList [("a", Number 4)]
      in processVars (Var "a") vars @?== (CalcResult 4, Env vars Map.empty)

eql = processVars [m|a = 4|] vars @?== (VarResult {
          answer = 4, 
          name = "a",
          value = vars Map.! "a",
          boundResults = Map.empty
      }, Env vars Map.empty)
      where vars = Map.fromList [("a", Number 4)]

bind = processVars [m|a + 2|] vars @?== (CalcResult 4, Env vars Map.empty)
       where vars = Map.fromList [("b", Number 2), ("a", Var "b")]

bindStmt = processVars [m|a := b|] vars @?== (VarResult {
               answer = 2, 
               name = "a",
               value = Var "b",
               boundResults = Map.empty
           }, Env vars Map.empty)
           where vars = Map.fromList [("b", Number 2), ("a", Var "b")]

errorVar = assertThrows "Use of undefined variable \"a\"" $ process (Var "a")

errorFunc = assertThrows "Use of undefined function \"foo\"" $ process [m|foo(2)|]

errorBuiltinFuncArgs = assertThrows "Unexpected number of arguments" $
                                    process [m|sin(2, 3)|]

errorFuncArgs = assertThrows "Unexpected number of arguments" $
                             processFuncs [m|foo(2, 3)|] fooFunc
                where fooFunc = Map.fromList [("foo", Function ["a"] (Var "a"))]

errorAST = assertThrows ("Cannot evaluate the statement " ++ show ast) $ process ast
           where ast = EqlStmt (Number 2) (Number 3)

functionDef = processFuncs [m|foo(b) = b + 2|] Map.empty @?==
                (FuncResult { name = "foo", def = fooFunc }, emptyEnv)
              where fooFunc = Function ["b"] [m|2 + b|]

function = process [m|sin(1)|] @?== CalcResult (sin 1)

userFunction = processFuncs [m|foo(2)|] fooFunc @?== (CalcResult 4, Env Map.empty fooFunc)
               where fooFunc = Map.fromList [("foo", Function ["b"] [m|b + 2|])]

nestedFunction = (fst <$> evalPass [m|f(2)|] defaultPrefs env) @?== CalcResult 2
                 where f = ("f", Function ["a"] [m|z(a)|])
                       z = ("z", Function ["a"] (Var "a"))
                       env = Env Map.empty $ Map.fromList [f, z]

-- The local x variable should be used when evaluating f
-- and the global x should be used when evaluating inner function m
varScopes = (fst <$> evalPass [m|f(2)|] defaultPrefs env) @?== CalcResult 13
            where f = ("f", Function ["x"] [m|x + z(3)|])
                  z = ("z", Function ["a"] [m|x + a|])
                  env = Env (Map.fromList [("x", Number 8)]) $ Map.fromList [f, z]
