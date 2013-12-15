module Calculator.Scanner.Tests (
    tests
) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Control.Exception(ErrorCall(..), evaluate)
import Calculator.Scanner
import Calculator.Data.Token()
import Test.HUnit.Tools

instance Eq ErrorCall where
    x == y = show x == show y

tests = [ testGroup "Simple" [
            testCase "Int" scanInt,
            testCase "Operators" scanOps,
            testCase "Decimal" scanDec,
            testCase "Exp" scanExp,
            testCase "Decimal with Exp" scanDecExp,
            testCase "Id" scanId,
            testCase "Brackets" scanBrack,
            testCase "Whitespace" scanWhitespace,
            testCase "Functions" scanFuncs
            ]
        , testGroup "Simple - Errors" [
            testCase "Int-Id" scanIntId,
            testCase "Dec-Id" scanDecId,
            testCase "Exp-Id" scanExpId,
            testCase "Exp-Id 2" scanExpId2,
            testCase "Id-Int" scanIdInt,
            testCase "Id-Decimal" scanIdDec,
            testCase "Double decimal" scanDecDec,
            testCase "Exp with decimal" scanExpDec,
            testCase "Exp-Exp" scanExpExp,
            testCase "Unfinished exp" scanUnfinExp,
            testCase "Double op" scanOpOp,
            testCase "Op minus op" scanOpNegOp
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
    , Token Numeric "0.8" ]

scanExp = scan "9e10" @?= [Token Numeric "9e10"]
scanDecExp = scan "2.3e2" @?= [Token Numeric "2.3e2"]
scanId = scan "thisisateststring" @?= [Token Id "thisisateststring"]
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

scanIntId = assertRaises "" (ErrorCall "ERROR: Invalid symbol a") (evaluate $ scan "123abc")

scanDecId = assertRaises "" (ErrorCall "ERROR: Invalid symbol a") (evaluate $ scan "1.23abc")

scanExpId = assertRaises "" (ErrorCall "ERROR: Invalid symbol a") (evaluate $ scan "12e3abc")

scanExpId2 = assertRaises "" (ErrorCall "ERROR: Left to parse ST_E abc") (evaluate $ scan "12eabc")

scanIdInt = assertRaises "" (ErrorCall "ERROR: Invalid symbol 1") (evaluate $ scan "abc123")

scanIdDec = assertRaises "" (ErrorCall "ERROR: Invalid symbol .") (evaluate $ scan "abc.234")

scanDecDec = assertRaises "" (ErrorCall "ERROR: Invalid symbol .") (evaluate $ scan "12.34.56")

scanExpDec = assertRaises "" (ErrorCall "ERROR: Invalid symbol .") (evaluate $ scan "12e3.4")
scanExpExp = assertRaises "" (ErrorCall "ERROR: Invalid symbol e") (evaluate $ scan "12e42e9")
scanUnfinExp = assertRaises "" (ErrorCall "ERROR: unexpected end of string") (evaluate $ scan "123e")
scanOpOp = assertRaises "" (ErrorCall "ERROR: Invalid symbol *") (evaluate $ scan "+*")
scanOpNegOp = assertRaises "" (ErrorCall "ERROR: Invalid symbol /") (evaluate $ scan "*-/")

scanBig = scan "(8*9e10+(7.289 / 3) % x -10)" @?=
    [ Token Lparen "("
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
