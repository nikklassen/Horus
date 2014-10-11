{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.Canon.Tests (
    tests
) where

import Calculator.Data.AST
import Calculator.Canon
import Calculator.Canon.Internal
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

tests = [ testCase "Number" number
        , testCase "Var" variable
        , testGroup "Can reduce" [
            testCase "Neg" negativeReduce
            , testCase "Func" funcReduce
            , testCase "FuncExpr" funcExprReduce
            ]
        , testCase "Func - partial reduce" funcPartial
        , testGroup "Can't reduce" [
            testCase "Neg" negativeNoReduce
            , testCase "Transcendental" transNoReduce
            , testCase "Large integer" bigIntNoReduce
            ]
        , testGroup "Mult" [
            testCase "Reduce consts" multReduceConsts
            , testCase "Assoc left" multAssocLeft
            , testCase "Mult over add with const - Num on right" multOverConstAddRight
            , testCase "Mult over add with const - Num on left" multOverConstAddLeft
            , testCase "Mult over add - Num on right" multOverAddRight
            , testCase "Mult over add - Num on left" multOverAddLeft
            , testCase "Mult over sub - Num on right" multOverSubRight
            , testCase "Mult over sub - Num on left" multOverSubLeft
            , testCase "Mult over gen add - Right" multOverGenAddRight
            , testCase "Mult over gen add - Left" multOverGenAddLeft
            , testCase "Mult over gen sub - Right" multOverGenSubRight
            , testCase "Mult over gen sub - Left" multOverGenSubLeft
            , testCase "Mult cycle left" multCycleLeft
            , testCase "Mult cycle right" multCycleRight
            , testCase "Mult move num left" multMoveNum
            , testCase "Mult other" multOther
            ]
        , testGroup "Add" [
            testCase "Reduce consts" addReduceConsts
            , testCase "Add cycle left" addCycleLeft
            , testCase "Add cycle right" addCycleRight
            , testCase "Add shift left" addShiftLeft
            , testCase "Add assoc left" addAssocLeft
            ]
        , testGroup "Sub" [
            testCase "Reduce consts" subReduceConsts
            , testCase "Sub to add" subToAdd
            ]
        , testGroup "Other ops" [
            testCase "Can reduce" opReduce
            , testCase "Can't reduce" opNoReduce
            ]
        , testGroup "Stmts" [
            testCase "Eql" eqlReduce
            , testCase "Bind" bindReduce
            ]
        ]

number = canon (Number 2) @?= Number 2

variable = canon (Var "a") @?= Var "a"

negativeReduce = canon (Neg (Number 2)) @?= Number (-2)

negativeNoReduce = canon (Neg (Var "a")) @?= Neg (Var "a")

funcReduce = canon (FuncExpr "cos" [OpExpr "+" (Number 1) (Number 1)]) @?= FuncExpr "cos" [Number 2]

funcExprReduce = canon (EqlStmt (FuncExpr "f" [Var "a"]) (OpExpr "+" (Number 1) (Number 1)))
                     @?= EqlStmt (FuncExpr "f" [Var "a"]) (Number 2)

funcPartial = canon (FuncExpr "foo" [opExpr, Var "a"]) @?= FuncExpr "foo" [Number 5, Var "a"]
              where opExpr = OpExpr "+" (Number 2) (Number 3)

transNoReduce = canon opExpr @?= opExpr
                where opExpr = FuncExpr "sin" [Number 1]

bigIntNoReduce = canon opExpr @?= opExpr
                 where opExpr = OpExpr "*" (Number 1e41) (Number 1e41)

multReduceConsts = canon (OpExpr "*" (Number 2) (Number 3)) @?= Number 6
multAssocLeft = canon (OpExpr "*" (Var "a") (OpExpr "*" (Var "b") (Var "c"))) @?= OpExpr "*" (OpExpr "*" (Var "a") (Var "b")) (Var "c")
multOverConstAddRight = canon (OpExpr "*" (OpExpr "+" (Number 2) (Var "a")) (Number 3)) @?= OpExpr "+" (Number 6) (OpExpr "*" (Number 3) (Var "a"))
multOverConstAddLeft = canon (OpExpr "*" (Number 3) (OpExpr "+" (Number 2) (Var "a"))) @?= OpExpr "+" (Number 6) (OpExpr "*" (Number 3) (Var "a"))
multOverAddRight = canon (OpExpr "*" (Number 3) (OpExpr "+" (Var "a") (Var "b"))) @?= OpExpr "+" (OpExpr "*" (Number 3) (Var "a")) (OpExpr "*" (Number 3) (Var "b"))
multOverAddLeft = canon (OpExpr "*" (OpExpr "+" (Var "a") (Var "b")) (Number 3)) @?= OpExpr "+" (OpExpr "*" (Number 3) (Var "a")) (OpExpr "*" (Number 3) (Var "b"))
multOverSubRight = canon (OpExpr "*" (Number 3) (OpExpr "-" (Var "a") (Var "b"))) @?= OpExpr "-" (OpExpr "*" (Number 3) (Var "a")) (OpExpr "*" (Number 3) (Var "b"))
multOverSubLeft = canon (OpExpr "*" (OpExpr "-" (Var "a") (Var "b")) (Number 3)) @?= OpExpr "-" (OpExpr "*" (Number 3) (Var "a")) (OpExpr "*" (Number 3) (Var "b"))
multOverGenAddRight = canon (OpExpr "*" (Var "c") (OpExpr "+" (Var "a") (Var "b"))) @?= OpExpr "+" (OpExpr "*" (Var "c") (Var "a")) (OpExpr "*" (Var "c") (Var "b"))
multOverGenAddLeft = canon (OpExpr "*" (OpExpr "+" (Var "a") (Var "b")) (Var "c")) @?= OpExpr "+" (OpExpr "*" (Var "a") (Var "c")) (OpExpr "*" (Var "b") (Var "c"))
multOverGenSubRight = canon (OpExpr "*" (Var "c") (OpExpr "-" (Var "a") (Var "b"))) @?= OpExpr "-" (OpExpr "*" (Var "c") (Var "a")) (OpExpr "*" (Var "c") (Var "b"))
multOverGenSubLeft = canon (OpExpr "*" (OpExpr "-" (Var "a") (Var "b")) (Var "c")) @?= OpExpr "-" (OpExpr "*" (Var "a") (Var "c")) (OpExpr "*" (Var "b") (Var "c"))
multCycleLeft = canon (OpExpr "*" (OpExpr "*" (Number 2) (Var "a")) (Number 3)) @?= OpExpr "*" (Number 6) (Var "a")
multCycleRight = canon (OpExpr "*" (Number 3) (OpExpr "*" (Number 2) (Var "a"))) @?= OpExpr "*" (Number 6) (Var "a")
multMoveNum = canon (OpExpr "*" (Var "a") (Number 3)) @?= OpExpr "*" (Number 3) (Var "a")
multOther = canon (OpExpr "*" (Number 2) (Var "a")) @?= OpExpr "*" (Number 2) (Var "a")

addReduceConsts = canon (OpExpr "+" (Number 2) (Number 3)) @?= Number 5
addCycleLeft = canon (OpExpr "+" (OpExpr "+" (Number 2) (Var "a")) (Number 3)) @?= OpExpr "+" (Number 5) (Var "a")
addCycleRight = canon (OpExpr "+" (Number 3) (OpExpr "+" (Number 2) (Var "a"))) @?= OpExpr "+" (Number 5) (Var "a")
addShiftLeft = canon (OpExpr "+" (Var "a") (Number 2)) @?= OpExpr "+" (Number 2) (Var "a")
addAssocLeft = canon (OpExpr "+" (Var "a") (OpExpr "+" (Var "b") (Var "c"))) @?= OpExpr "+" (OpExpr "+" (Var "a") (Var "b")) (Var "c")

subReduceConsts = canon (OpExpr "-" (Number 3) (Number 2)) @?= Number 1
subToAdd = canon (OpExpr "-" (Var "a") (Number 2)) @?= OpExpr "+" (Number (-2)) (Var "a")

opReduce = canon (OpExpr "%" (Number 8) (Number 6)) @?= Number 2
opNoReduce = canon opExpr @?= opExpr
             where opExpr = OpExpr "/" (Var "a") (Number 3)

eqlReduce = canonPass (EqlStmt (Var "b") (OpExpr "+" (Number 2) (Number 3))) @?= EqlStmt (Var "b") (Number 5)

bindReduce = canonPass (BindStmt (Var "b") (OpExpr "+" (Number 2) (Number 3))) @?= BindStmt (Var "b") (Number 5)
