{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.Functions.Tests where

import Calculator.Data.AST
import Calculator.Functions
import Data.Number.CReal
import Data.Maybe (fromJust)
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestHelpers

tests = [ testCase "Show declaration" showDecl
        , testCase "Declaration contains non-variable" declareWithNonVar
        , testCase "Integral function" integralFunc
        , testCase "Recognizes functions" isFunctionTrue
        , testCase "Recognizes non function" isFunctionFalse
        , testCase "Converts degrees" degrees
        , testGroup "Factorial"
            [ testCase "Non-integer" factNonInteger
            , testCase "Negative" factNegative
            , testCase "Factorial" factorial
            ]
        , testCase "Root" root
        , testCase "Log with base" logWithBase
        , testCase "Log no base" logNoBase
        ]

func :: String -> [CReal] -> CReal
func = fromJust . getFunction

showDecl = showDeclaration (Function ["a", "b"] (Number 2)) @?= "(a, b)"

declareWithNonVar = deepAssertRaises "Unexpected expression \"2.0\" in parameter list" $ buildFunction [Var "a", Number 2] (Number 2)

integralFunc = func "ceil" [1.23] @?= (2 :: CReal)

isFunctionTrue = isFunction "sin" @?= True

isFunctionFalse = isFunction "notAFunction" @?= False

degrees = func "deg" [180] @?= (pi :: CReal)

factNonInteger = deepAssertRaises "Factorial can only be applied to non-negative integers" $
                                  func "fact" [1.23]

factNegative = deepAssertRaises "Factorial can only be applied to non-negative integers" $
                                func "fact" [-1]

factorial = func "fact" [4] @?= 24

root = func "root" [3, 8] @?= 2

logWithBase = func "log" [2, 8] @?= 3

logNoBase = func "log" [100] @?= 2
