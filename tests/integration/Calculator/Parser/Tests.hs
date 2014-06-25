{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calculator.Parser.Tests (
    tests
) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Control.Exception (ErrorCall(..), evaluate, catch)
import Calculator.Data.AST
import Calculator.Parser

assertException :: IO AST -> Assertion
assertException action =
    (do
        result <- action
        assertFailure $ "Expected exception, got " ++ show result)
    `Control.Exception.catch` \ (_ :: ErrorCall) -> assertBool "" True

tests = [ testGroup "Simple" [
            testCase "Int" parseInt,
            testCase "Decimal" parseDec,
            testCase "Exp" parseExp,
            testCase "Decimal with Exp" parseDecExp,
            testCase "Id" parseId,
            testCase "Brackets" parseBrack,
            testCase "Whitespace" parseWhitespace,
            testCase "Functions" parseFuncs,
            testCase "Function - multiple params" parseFuncMulti,
            testCase "Function definition" parseFuncDef,
            testCase "Equals sign" parseEqls,
            testCase "Negative" parseNeg
            ]
        , testGroup "Simple - Errors" [
            testCase "Int-Id" parseIntId,
            testCase "Dec-Id" parseDecId,
            testCase "Exp-Id" parseExpId,
            testCase "Exp-Id 2" parseExpId2,
            testCase "Id-Decimal" parseIdDec,
            testCase "Double decimal" parseDecDec,
            testCase "Exp with decimal" parseExpDec,
            testCase "Exp-Exp" parseExpExp,
            testCase "Unfinished exp" parseUnfinExp,
            testCase "Double term" parseDoubleTerm,
            testCase "Equals number" parseEqlNum,
            testCase "Mismatched parens" parseMismatchParen,
            testCase "Missing paren" parseMissingParen,
            testCase "Equality, missing left" parseEqlMissingLeft, 
            testCase "Equality, missing right" parseEqlMissingRight
            ]
        , testCase "Big" parseBig
        ]

parseInt = parse "0123456789876543210" @?= Number "0123456789876543210"

parseDec = parse ".9" @?= Number ".9"

parseExp = parse "9e10 / 9e-10" @?= OpExpr "/" (Number "9e10") (Number "9e-10")

parseDecExp = parse "2.3e2" @?= Number "2.3e2"

parseId = parse "thisisateststring * abc123" @?= OpExpr "*" (Var "thisisateststring") (Var "abc123")

parseBrack = parse "(2) * [3]" @?= OpExpr "*" (Number "2") (Number "3")

parseWhitespace = parse "         1         " @?= Number "1"

parseFuncs = parse "sin(1)" @?= FuncExpr "sin" [Number "1"]

parseFuncMulti = parse "f( a , 2 )" @?= FuncExpr "f" [Var "a", Number "2"]

parseFuncDef = parse "f(a) = a + 2" @?= EqlStmt (FuncExpr "f" [Var "a"]) (OpExpr "+" (Var "a") (Number "2"))

parseEqls = parse "a = 1" @?= EqlStmt (Var "a") (Number "1")

parseNeg = parse "1 / -2" @?= OpExpr "/" (Number "1") (Neg (Number "2"))

parseIntId = assertException (evaluate $ parse "123abc")

parseDecId = assertException (evaluate $ parse "1.23abc")

parseExpId = assertException (evaluate $ parse "12e3abc")

parseExpId2 = assertException (evaluate $ parse "12eabc")

parseIdDec = assertException (evaluate $ parse "abc.234")

parseDecDec = assertException (evaluate $ parse "12.34.56")

parseExpDec = assertException (evaluate $ parse "12e3.4")

parseExpExp = assertException (evaluate $ parse "12e42e9")

parseUnfinExp = assertException (evaluate $ parse "123e")

parseDoubleTerm = assertException (evaluate $ parse "12 34")

parseEqlNum = assertException (evaluate $ parse "1 = 2")

parseEqlMissingLeft = assertException (evaluate $ parse "a =")

parseEqlMissingRight = assertException (evaluate $ parse "= a")

parseMismatchParen = assertException (evaluate $ parse "(((2))]")

parseMissingParen = assertException (evaluate $ parse "(((2))")

parseBig = parse "c=(8*9e10+(7.289 / 3) % x -10)" @?=
    EqlStmt (Var "c")
            (OpExpr "%" (OpExpr "+" (OpExpr "*" (Number "8")
                                                (Number "9e10"))
                                    (OpExpr "/" (Number "7.289")
                                                (Number "3")))
                        (OpExpr "-" (Var "x")
                                    (Number "10")))
