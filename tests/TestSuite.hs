module Main where

import Test.Framework(defaultMain, testGroup)
import qualified Calculator.Parser.Tests as Parser
import qualified Calculator.Evaluator.Tests as Evaluator

main = defaultMain [
    testGroup "Calculator.Parser.Tests" Parser.tests,
    testGroup "Calculator.Evaluator.Tests" Evaluator.tests
    ]
