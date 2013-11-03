import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Calculator.Scanner

main = defaultMain tests

tests = [ testGroup "Simple" [
            testCase "Int" scan_int,
            testCase "Operators" scan_ops,
            testCase "Decimal" scan_dec,
            testCase "Exp" scan_exp,
            testCase "Decimal with Exp" scan_dec_exp,
            testCase "Id" scan_id,
            testCase "Brackets" scan_brack
            ]
        , testGroup "Simple - Errors" [
            testCase "Int-Id" scan_int_id,
            testCase "Dec-Id" scan_dec_id,
            testCase "Exp-Id" scan_exp_id,
            testCase "Id-Int" scan_id_int,
            testCase "Double decimal" scan_dec_dec,
            testCase "Exp with decimal" scan_exp_dec,
            testCase "Exp-Exp" scan_exp_exp,
            testCase "Double op" scan_op_op
            ]
        , testCase "Big" scan_big
        ]

scan_int = scan "0123456789876543210" @?= Right [Token Numeric "0123456789876543210"]

scan_ops = scan "- * / + ^ %" @?=
    Right [Token Op "-"
    , Token Op "*"
    , Token Op "/"
    , Token Op "+"
    , Token Op "^"
    , Token Op "%"
    ]

scan_dec = scan "0.9" @?= Right [Token Numeric "0.9"]
scan_exp = scan "9e10" @?= Right [Token Numeric "9e10"]
scan_dec_exp = scan "2.3e2" @?= Right [Token Numeric "2.3e2"]
scan_id = scan "thisisateststring" @?= Right [Token Id "thisisateststring"]
scan_brack = scan "()" @?=
    Right [Token Lparen "("
    , Token Rparen ")"
    ]

scan_int_id = scan "123abc" @?= Left "ERROR: Invalid symbol a"
scan_dec_id = scan "12.3abc" @?= Left "ERROR: Invalid symbol a"
scan_exp_id = scan "12e3abc" @?= Left "ERROR: Invalid symbol a"
scan_id_int = scan "abc123" @?= Left "ERROR: Invalid symbol 1"
scan_dec_dec = scan "12.34.56" @?= Left "ERROR: Invalid symbol ."
scan_exp_dec = scan "12e3.4" @?= Left "ERROR: Invalid symbol ."
scan_exp_exp = scan "12e42e9" @?= Left "ERROR: Invalid symbol e"
scan_op_op = scan "+-" @?= Left "ERROR: Invalid symbol -"

scan_big = scan "(8*9e10+(7.289 / 3) % x -10)" @?=
    Right [Token Lparen "("
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
