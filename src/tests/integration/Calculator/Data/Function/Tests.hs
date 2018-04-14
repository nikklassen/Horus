module Calculator.Data.Function.Tests where

import Calculator.Data.AST
import Calculator.Data.Function
import Test.HUnit

tests = [ testCase "Show declaration" showDecl
        , testCase "Declaration contains non-variable" declareWithNonVar
        ]

showDecl = showDeclaration (Function ["a", "b"] (Number 2)) @?= "(a, b)"

declareWithNonVar = deepAssertRaises "Unexpected expression \"2.0\" in parameter list" $ buildFunction [Var "a", Number 2] (Number 2)
