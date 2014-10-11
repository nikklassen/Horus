module Main where

import Test.Framework(defaultMain, testGroup)
import qualified Calculator.Canon.Tests as Canon
import qualified Calculator.Evaluator.Tests as Evaluator
import qualified Calculator.Parser.Tests as Parser
import qualified Calculator.SynCheck.Tests as SynCheck

main = defaultMain [
    testGroup "Calculator.Canon.Tests" Canon.tests,
    testGroup "Calculator.Evaluator.Tests" Evaluator.tests,
    testGroup "Calculator.Parser.Tests" Parser.tests,
    testGroup "Calculator.SynCheck.Tests" SynCheck.tests
    ]
