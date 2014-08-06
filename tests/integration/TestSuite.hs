module Main where

import Test.Framework(defaultMain, testGroup)
import qualified Calculator.Parser.Tests as Parser
import qualified Calculator.Evaluator.Tests as Evaluator
import qualified Calculator.Canon.Tests as Canon

main = defaultMain [
    testGroup "Calculator.Parser.Tests" Parser.tests,
    testGroup "Calculator.Evaluator.Tests" Evaluator.tests,
    testGroup "Calculator.Canon.Tests" Canon.tests
    ]
