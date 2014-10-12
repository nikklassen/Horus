{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.Evaluator.Tests (
    tests
) where

import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.Evaluator
import Calculator.Functions
import Control.Exception (ErrorCall(..), evaluate)
import Data.Map (Map)
import Data.Number.CReal
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
            , testCase "Function" function
            , testCase "User function" userFunction
            , testCase "Function definition" functionDef
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
        ]

process :: AST -> CReal
process ast = fst $ evalPass ast $ Env Map.empty Map.empty

processVars :: AST -> Map String AST -> (CReal, Env)
processVars ast vars = evalPass ast $ Env vars Map.empty

processFuncs :: AST -> Map String Function -> (CReal, Env)
processFuncs ast funcs = evalPass ast $ Env Map.empty funcs

int = process (Number 0123456789876543210) @?= 123456789876543210

dec = process (Number 0.8) @?= 0.8

exponential = process (Number 9e-10) @?= 9e-10

decExp = process (Number 2.3e2) @?= 230

op = process (OpExpr "+" (Number 1)
                         (Number 1)) @?= 2

realMod = process (OpExpr "%" (Number 4)
                              (Number 1.5)) @?= 1

negToPower = process (OpExpr "^" (Number (-3))
                                 (Number (-2))) @?= -1/9

neg = process (Neg (Number 2)) @?= -2

function = process (FuncExpr "sin" [Number 1]) @?= sin 1

userFunction = processFuncs (FuncExpr "foo" [Number 2]) fooFunc @?= (4, Env Map.empty fooFunc)
               where fooFunc = Map.fromList [("foo", Function ["b"] (OpExpr "+" (Var "b") (Number 2)))]

functionDef = processFuncs (EqlStmt (FuncExpr "foo" [Var "b"]) (OpExpr "+" (Var "b") (Number 2))) Map.empty @?= (0, Env Map.empty fooFunc)
              where fooFunc = Map.fromList [("foo", Function ["b"] (OpExpr "+" (Number 2) (Var "b")))]

var = let vars = Map.fromList [("a", Number 4)]
      in processVars (Var "a") vars @?= (4, Env vars Map.empty)

eql = processVars (EqlStmt (Var "a") (Number 4)) (Map.fromList [("a", Number 2)]) @?= (4, Env (Map.fromList [("a", Number 4)]) Map.empty)

bind = processVars (OpExpr "+" (Var "a") (Number 2)) vars @?= (4, Env vars Map.empty)
       where vars = Map.fromList [("b", Number 2), ("a", Var "b")]

bindStmt = processVars (BindStmt (Var "a") (Var "b")) vars @?= (2, Env vars Map.empty)
           where vars = Map.fromList [("b", Number 2), ("a", Var "b")]

errorVar = assertRaises "" (ErrorCall "Use of undefined variable \"a\"")
                           (evaluate $ process (Var "a"))

errorFunc = assertRaises "" (ErrorCall "Use of undefined function \"foo\"")
                            (evaluate $ process (FuncExpr "foo" [Number 2]))

errorBuiltinFuncArgs = assertRaises "" (ErrorCall "Unexpected number of arguments")
                                       (evaluate $ process (FuncExpr "sin" [Number 2, Number 3]))

errorFuncArgs = assertRaises "" (ErrorCall "Unexpected number of arguments")
                                -- Use some empty variables to force the evaluation of the expression
                                (processFuncs (FuncExpr "foo" [Number 2, Number 3]) fooFunc @?= (4, Env Map.empty Map.empty))
                                where fooFunc = Map.fromList [("foo", Function ["a"] (Var "a"))]

errorAST = assertRaises "" (ErrorCall $ "Cannot evaluate the statement " ++ show ast)
                           (evaluate $ process ast)
                           where ast = EqlStmt (Number 2) (Number 3)
