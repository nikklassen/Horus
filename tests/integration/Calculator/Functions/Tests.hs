{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.Functions.Tests where

import Calculator.Data.Decimal
import Calculator.Data.Env
import Calculator.Functions
import Data.Maybe (fromJust)
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestHelpers

tests = [ testCase "Integral function" integralFunc
        , testCase "Recognizes functions" isFunctionTrue
        , testCase "Recognizes non function" isFunctionFalse
        , testGroup "Angles"
            [ testCase "Degrees" degrees
            , testCase "Radians" radians
            ]
        , testGroup "Factorial"
            [ testCase "Non-integer" factNonInteger
            , testCase "Negative" factNegative
            , testCase "Factorial" factorial
            ]
        , testCase "Root" root
        , testCase "Log with base" logWithBase
        , testCase "Log no base" logNoBase
        ]

func :: String -> [Decimal] -> Decimal
func fName = fromJust $ getFunction fName defaultPrefs

integralFunc = func "ceil" [1.23] @?= (2 :: Decimal)

isFunctionTrue = isFunction "sin" defaultPrefs @?= True

isFunctionFalse = isFunction "notAFunction" defaultPrefs @?= False

degrees = sinEqualsOne 90 $ defaultPrefs { isRadians = False }

radians = sinEqualsOne ((pi :: Decimal) / 2) $ defaultPrefs { isRadians = True }

sinEqualsOne angle prefs = (fromJust $ getFunction "sin" prefs) [angle] @?= (1 :: Decimal)

factNonInteger = deepAssertRaises "Factorial can only be applied to non-negative integers" $
                                  func "fact" [1.23]

factNegative = deepAssertRaises "Factorial can only be applied to non-negative integers" $
                                func "fact" [-1]

factorial = func "fact" [4] @?= 24

root = func "root" [3, 8] @?= 2

logWithBase = func "log" [2, 8] @?= 3

logNoBase = func "log" [100] @?= 2
