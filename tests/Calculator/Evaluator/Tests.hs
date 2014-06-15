{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.Evaluator.Tests (
    tests
) where

import Calculator.Evaluator
import Calculator.Data.AST
import Calculator.Functions

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Tools
import Control.Exception (ErrorCall(..), evaluate)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Data.Number.CReal

tests = [ testGroup "Simple"
            [ testCase "Int" int
            , testCase "Decimal" dec
            , testCase "Exponent" exponential
            , testCase "Decimal with exponent" decExp
            , testCase "Operator" op
            , testCase "Real mod" realMod
            , testCase "Negative number with power" negToPower
            , testCase "Negate" neg
            , testCase "Function" function
            , testCase "User function" userFunction
            , testCase "Function definition" functionDef
            , testCase "Var" var
            , testCase "Assignment" eql
            ]
        , testGroup "Error"
            [ testCase "Undefined var" errorVar
            , testCase "Undefined function" errorFunc
            , testCase "Builtin function - wrong number of args" errorBuiltinFuncArgs
            , testCase "User function - wrong number of args" errorFuncArgs
            , testCase "Invalid AST" errorAST
            ]
        ]

process :: AST -> CReal
process ast = evalState (eval ast) $ Env Map.empty Map.empty

processVars :: AST -> Map String CReal -> (CReal, Env)
processVars ast vars = runState (eval ast) $ Env vars Map.empty

processFuncs :: AST -> Map String Function -> (CReal, Env)
processFuncs ast funcs = runState (eval ast) $ Env Map.empty funcs

int = process (Number "0123456789876543210") @?= 123456789876543210

dec = process (Number ".8") @?= 0.8

exponential = process (Number "9e-10") @?= 9e-10

decExp = process (Number "2.3e2") @?= 230

op = process (OpExpr "+" (Number "1")
                         (Number "1")) @?= 2

realMod = process (OpExpr "%" (Number "4")
                              (Number "1.5")) @?= 1

negToPower = process (OpExpr "^" (Number "-3")
                                 (Number "-2")) @?= -1/9

neg = process (Neg (Number "2")) @?= -2

function = process (FuncExpr "sin" [Number "1"]) @?= sin 1

userFunction = processFuncs (FuncExpr "foo" [Number "2"]) fooFunc @?= (4, Env Map.empty fooFunc)
               where fooFunc = Map.fromList [("foo", Function ["b"] (OpExpr "+" (Var "b") (Number "2")))]

functionDef = processFuncs (EqlStmt (FuncExpr "foo" [Var "b"]) (OpExpr "+" (Var "b") (Number "2"))) Map.empty @?= (0, Env Map.empty fooFunc)
              where fooFunc = Map.fromList [("foo", Function ["b"] (OpExpr "+" (Var "b") (Number "2")))]

var = let vars = Map.fromList [("a", 4)]
      in processVars (Var "a") vars @?= (4, Env vars Map.empty)

eql = processVars (EqlStmt (Var "a") (Number "4")) (Map.fromList [("a", 2)]) @?= (4, Env (Map.fromList [("a", 4)]) Map.empty)

errorVar = assertRaises "" (ErrorCall "Use of undefined variable \"a\"")
                           (evaluate $ process (Var "a"))

errorFunc = assertRaises "" (ErrorCall "Use of undefined function \"foo\"")
                            (evaluate $ process (FuncExpr "foo" [Number "2"]))

errorBuiltinFuncArgs = assertRaises "" (ErrorCall "Unexpected number of arguments")
                                       (evaluate $ process (FuncExpr "sin" [Number "2", Number "3"]))

errorFuncArgs = assertRaises "" (ErrorCall "Unexpected number of arguments")
                                -- Use some empty variables to force the evaluation of the expression
                                (processFuncs (FuncExpr "foo" [Number "2", Number "3"]) fooFunc @?= (4, Env Map.empty Map.empty))
                                where fooFunc = Map.fromList [("foo", Function ["a"] (Var "a"))]

errorAST = assertRaises "" (ErrorCall $ "Unexpected AST " ++ show ast)
                           (evaluate $ process ast)
                           where ast = EqlStmt (Number "2") (Number "3")
