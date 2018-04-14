{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.Canon.Tests (
    tests
) where

import Calculator.Data.AST
import Calculator.Canon
import Calculator.Canon.Internal
import Calculator.Parser
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import TestHelpers

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
        , testGroup "Add"
            [ testCase "Reduce consts" addReduceConsts
            , testCase "Add cycle left" addCycleLeft
            , testCase "Add cycle right" addCycleRight
            , testCase "Add shift left" addShiftLeft
            , testCase "Add assoc left" addAssocLeft
            ]
        , testGroup "Sub"
            [ testCase "Reduce consts" subReduceConsts
            , testCase "Sub to add" subToAdd
            ]
        , testGroup "Other ops"
            [ testCase "Can reduce" opReduce
            , testCase "Can't reduce" opNoReduce
            ]
        , testGroup "Stmts"
            [ testCase "Eql" eqlReduce
            , testCase "Bind" bindReduce
            ]
        ]

number = canon [m|2|] @?== [m|2|]

variable = canon [m|a|] @?== [m|a|]

negativeReduce = canon (Neg (Number 2)) @?== Number (-2)

negativeNoReduce = canon [m|-a|] @?== [m|-a|]

funcReduce = canon [m|cos(1+1)|] @?== [m|cos(2)|]

funcExprReduce = canon [m|f(a) = 1 + 1|] @?== [m|f(a) = 2|]

funcPartial = canon [m|foo(2 + 3, a)|] @?== [m|foo(5, a)|]

transNoReduce = canon [m|sin(1)|] @?== [m|sin(1)|]

bigIntNoReduce = canon [m|1e41 * 1e41|] @?== [m|1e41 * 1e41|]

-- Multiplication

multReduceConsts = canon [m|2 * 3|] @?== [m|6|]

multAssocLeft = canon [m|a * (b * c)|] @?== [m|(a * b) * c|]

multOverConstAddRight = canon [m|(2 + a) * 3|] @?== [m|6 + (3 * a)|]

multOverConstAddLeft = canon [m|3 * (2 + a)|] @?== [m|6 + (3 * a)|]

multOverAddRight = canon [m|3 * (a + b)|] @?== [m|(3 * a) + (3 * b)|]

multOverAddLeft = canon [m|(a + b) * 3|] @?== [m|(3 * a) + (3 * b)|]

multOverSubRight = canon [m|3 * (a - b)|] @?== [m|(3 * a) - (3 * b)|]

multOverSubLeft = canon [m|(a - b) * 3|] @?== [m|(3 * a) - (3 * b)|]

multOverGenAddRight = canon [m|c * (a + b)|] @?== [m|(c * a) + (c * b)|]

multOverGenAddLeft = canon [m|(a + b) * c|] @?== [m|(a * c) + (b * c)|]

multOverGenSubRight = canon [m|c * (a - b)|] @?== [m|(c * a) - (c * b)|]

multOverGenSubLeft = canon [m|(a - b) * c|] @?== [m|(a * c) - (b * c)|]

multCycleLeft = canon [m|(2 * a) * 3|] @?== [m|6 * a|]

multCycleRight = canon [m|3 * (2 * a)|] @?== [m|6 * a|]

multMoveNum = canon [m|a * 3|] @?== [m|3 * a|]

multOther = canon [m|2 * a|] @?== [m|2 * a|]

-- Addition

addReduceConsts = canon [m|2 + 3|] @?== [m|5|]

addCycleLeft = canon [m|(2 + a) + 3|] @?== [m|5 + a|]

addCycleRight = canon [m|3 + (2 + a)|] @?== [m|5 + a|]

addShiftLeft = canon [m|a + 2|] @?== [m|2 + a|]

addAssocLeft = canon [m|a + (b + c)|] @?== [m|(a + b) + c|]

-- Subtraction

subReduceConsts = canon [m|3 - 2|] @?== [m|1|]

subToAdd = canon [m|a - 2|] @?== OpExpr "+" (Number (-2)) (Var "a")

-- Other

opReduce = canon [m|8 % 6|] @?== [m|2|]

opNoReduce = canon [m|a / 3|] @?== [m|a / 3|]

eqlReduce = canonPass [m|b = 2 + 3|] @?== [m|b = 5|]

bindReduce = canonPass [m|b := 2 + 3|] @?== [m|b := 5|]
