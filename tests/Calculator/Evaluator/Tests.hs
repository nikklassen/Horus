module Calculator.Evaluator.Tests (
    tests
) where

import Calculator.Evaluator
import Calculator.Data.AST

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
            , testCase "Negate" neg
            , testCase "Function" function
            , testCase "Var" var
            , testCase "Assignment" eql
            ]
        , testGroup "Error"
            [ testCase "Undefined var" errorVar
            ]
        ]

process :: AST -> CReal
process ast = evalState (eval ast) (Map.fromList [])

processVars :: AST -> Map String CReal -> (CReal, Map String CReal)
processVars ast vars = runState (eval ast) vars

int = process (Number "0123456789876543210") @?= 123456789876543210

dec = process (Number ".8") @?= 0.8

exponential = process (Number "9e-10") @?= 9e-10

decExp = process (Number "2.3e2") @?= 230

op = process (OpExpr "+" (Number "1")
                         (Number "1")) @?= 2

realMod = process (OpExpr "%" (Number "4")
                              (Number "1.5")) @?= 1

neg = process (Neg (Number "2")) @?= -2

function = process (Function "sin" (Number "1")) @?= sin 1

var = let vars = Map.fromList [("a", 4)]
      in processVars (Var "a") vars  @?= (4, vars)

eql = processVars (EqlStmt (Var "a") (Number "4")) (Map.fromList [("a", 2)]) @?= (4, Map.fromList [("a", 4)])

errorVar = assertRaises "" (ErrorCall "Use of undefined variable \"a\"") (evaluate $ process (Var "a"))

errorFunc = assertRaises "" (ErrorCall "Use of undefined function \"foo\"")
                            (evaluate $ process (Function "foo" (Number "2")))
