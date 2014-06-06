module Calculator.Parser.Tests (
    tests
) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Control.Exception(ErrorCall(..), evaluate)
import Calculator.Lexer
import Test.HUnit.Tools

instance Eq ErrorCall where
    x == y = show x == show y

tests = []

{--
tests = [ testGroup "Fix Negatives" [
            testCase "Minus sign" fix_minus,
            testCase "Op & neg num" fix_op_neg,
            testCase "BOL neg num" fix_bol_neg,
            testCase "Neg num in brackets" fix_lparen_neg
            ]
        , testGroup "Parse" [
            testCase "Simple op, same precedence" parse_op_same_precedence,
            testCase "Simple mixed precedence" parse_mixed_precedence,
            testCase "Brackets w mixed precedence" parse_brackets_mixed,
            testCase "All operators" parse_big
            ]
        , testGroup "Parse Errors" [
            testCase "Missing left bracket" parse_missing_left,
            testCase "Missing right bracket" parse_missing_right
            ]
        , testGroup "Get Equation" [
            testCase "No equals sign" equation_no_eqls,
            testCase "Full equation" equation_lhs_and_rhs,
            testCase "Error: No LHS" equation_no_lhs
            ]
        ]

fix_minus =
    let tokens = [ Token Numeric "1" , Token Op "-" , Token Numeric "1" ]
    in fixNegs tokens @?= tokens

fix_bol_neg = fixNegs [ Token Op "-", Token Numeric "1" ] @?=
    [ Token Function "neg"
    , Token Numeric "1" ]

fix_op_neg = fixNegs [ Token Numeric "1", Token Op "+", Token Op "-", Token Numeric "1" ] @?=
    [ Token Numeric "1"
    , Token Op "+"
    , Token Function "neg"
    , Token Numeric "1" ]

fix_lparen_neg = fixNegs [ Token Lparen "(", Token Op "-", Token Numeric "1" ] @?=
    [ Token Lparen "("
    , Token Function "neg"
    , Token Numeric "1" ]

parse_op_same_precedence = parse [ Token Numeric "1", Token Op "+", Token Numeric "2", Token Op "-", Token Numeric "3" ] @?=
    [ Token Numeric "1"
    , Token Numeric "2"
    , Token Op "+"
    , Token Numeric "3"
    , Token Op "-" ]

parse_mixed_precedence = parse [ Token Numeric "1", Token Op "+", Token Numeric "2", Token Op "*", Token Numeric "3" ] @?=
    [ Token Numeric "1"
    , Token Numeric "2"
    , Token Numeric "3"
    , Token Op "*"
    , Token Op "+" ]

parse_brackets_mixed = parse [ Token Lparen "(", Token Numeric "1", Token Op "+", Token Numeric "2", Token Rparen ")", Token Op "*", Token Numeric "3" ] @?=
    [ Token Numeric "1"
    , Token Numeric "2"
    , Token Op "+"
    , Token Numeric "3"
    , Token Op "*" ]

parse_missing_left = assertRaises "" (ErrorCall "ERROR: mismatched brackets") (evaluate $ parse [ Token Numeric "1", Token Rparen ")" ])

parse_missing_right = assertRaises "" (ErrorCall "ERROR: mismatched brackets") (evaluate $ parse [ Token Lparen "(", Token Numeric "1" ])

parse_big = parse [Token Lparen "(", Token Numeric "2", Token Op "+", Token Numeric "3", Token Rparen ")", Token Op "/", Token Numeric "5", Token Op "^", Token Function "neg", Token Numeric "6", Token Op "*", Token Numeric "4", Token Op "%", Token Numeric "2", Token Op "-", Token Numeric "4"] @?=
    [ Token Numeric "2"
    , Token Numeric "3"
    , Token Op "+"
    , Token Numeric "5"
    , Token Numeric "6"
    , Token Function "neg"
    , Token Op "^"
    , Token Op "/"
    , Token Numeric "4"
    , Token Op "*"
    , Token Numeric "2"
    , Token Numeric "4"
    , Token Op "-"
    , Token Op "%"
    ]

equation_no_eqls = getEquation
    [ Token Numeric "1"
    , Token Op "+"
    , Token Numeric "2"
    ] @?=
        ( []
        , [ Token Numeric "1"
          , Token Op "+"
          , Token Numeric "2"
          ]
        )

equation_no_lhs = assertRaises "" (ErrorCall "ERROR: Expected lhs") $ evaluate $ getEquation
    [ Token Eql "="
    , Token Numeric "1"
    , Token Op "+"
    , Token Numeric "2"
    ]

equation_lhs_and_rhs = getEquation [ Token Id "a"
    , Token Eql "="
    , Token Numeric "1"
    , Token Op "+"
    , Token Numeric "2"
    ] @?=
        ( [ Token Id "a" ]
        , [ Token Numeric "1"
          , Token Op "+"
          , Token Numeric "2"
          ]
        )
--}
