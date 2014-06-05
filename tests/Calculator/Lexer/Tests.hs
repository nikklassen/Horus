{-# LANGUAGE ScopedTypeVariables #-}

module Calculator.Lexer.Tests (
    tests
) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Control.Exception (ErrorCall(..), evaluate, catch)
import Calculator.Lexer
import Calculator.Data.Token

instance Eq ErrorCall where
    x == y = show x == show y

assertException :: IO a -> Assertion
assertException action = (action >> assertFailure "") `Control.Exception.catch` \ (_ :: ErrorCall) -> assertBool "" True

tests = [ testGroup "Simple" [
            testCase "Int" scanInt,
            testCase "Operators" scanOps,
            testCase "Decimal" scanDec,
            testCase "Exp" scanExp,
            testCase "Decimal with Exp" scanDecExp,
            testCase "Id" scanId,
            testCase "Brackets" scanBrack,
            testCase "Whitespace" scanWhitespace,
            testCase "Functions" scanFuncs,
            testCase "Equals sign" scanEqls
            ]
        , testGroup "Simple - Errors" [
            testCase "Int-Id" scanIntId,
            testCase "Dec-Id" scanDecId,
            testCase "Exp-Id" scanExpId,
            testCase "Exp-Id 2" scanExpId2,
            testCase "Id-Decimal" scanIdDec,
            testCase "Double decimal" scanDecDec,
            testCase "Exp with decimal" scanExpDec,
            testCase "Exp-Exp" scanExpExp,
            testCase "Unfinished exp" scanUnfinExp
            ]
        , testCase "Big" scanBig
        , testCase "Add negative" scanAddNeg
        ]

scanInt = scan "0123456789876543210" @?= [Token Numeric "0123456789876543210"]

scanOps = scan "- * / + ^ %" @?=
    [ Token Op "-"
    , Token Op "*"
    , Token Op "/"
    , Token Op "+"
    , Token Op "^"
    , Token Op "%"
    ]

scanDec = scan "0.9 .8" @?=
    [ Token Numeric "0.9"
    , Token Numeric ".8" ]

scanExp = scan "9e10 9e-10" @?=
    [ Token Numeric "9e10"
    , Token Numeric "9e-10"
    ]

scanDecExp = scan "2.3e2" @?= [Token Numeric "2.3e2"]

scanId = scan "thisisateststring abc123" @?=
    [ Token Id "thisisateststring"
    , Token Id "abc123"
    ]

scanBrack = scan "()" @?=
    [ Token Lparen "("
    , Token Rparen ")" ]

scanWhitespace = scan "         1         " @?= [Token Numeric "1"]

scanFuncs = scan "sin cos tan asin acos atan" @?=
    [ Token Function "sin"
    , Token Function "cos"
    , Token Function "tan"
    , Token Function "asin"
    , Token Function "acos"
    , Token Function "atan"
    ]

scanEqls = scan "=" @?= [Token Eql "="]

scanIntId = assertException (evaluate $ scan "123abc")

scanDecId = assertException (evaluate $ scan "1.23abc")

scanExpId = assertException (evaluate $ scan "12e3abc")

scanExpId2 = assertException (evaluate $ scan "12eabc")

scanIdDec = assertException (evaluate $ scan "abc.234")

scanDecDec = assertException (evaluate $ scan "12.34.56")

scanExpDec = assertException (evaluate $ scan "12e3.4")

scanExpExp = assertException (evaluate $ scan "12e42e9")

scanUnfinExp = assertException (evaluate $ scan "123e")

scanBig = scan "c=(8*9e10+(7.289 / 3) % x -10)" @?=
    [ Token Id "c"
    , Token Eql "="
    , Token Lparen "("
    , Token Numeric "8"
    , Token Op "*"
    , Token Numeric "9e10"
    , Token Op "+"
    , Token Lparen "("
    , Token Numeric "7.289"
    , Token Op "/"
    , Token Numeric "3"
    , Token Rparen ")"
    , Token Op "%"
    , Token Id "x"
    , Token Op "-"
    , Token Numeric "10"
    , Token Rparen ")"
    ]

scanAddNeg = scan "2 + -3" @?=
    [ Token Numeric "2"
    , Token Op "+"
    , Token Op "-"
    , Token Numeric "3"
    ]
