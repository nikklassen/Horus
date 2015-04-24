module Main where

import Test.Framework(defaultMain, testGroup)
import qualified Calculator.Canon.Tests as Canon
import qualified Calculator.Evaluator.Tests as Evaluator
import qualified Calculator.Parser.Tests as Parser
import qualified Calculator.ASTPass.SynCheck.Tests as SynCheck
import qualified Calculator.Functions.Tests as Functions

main :: IO ()
main = defaultMain [
    testGroup "Calculator.Canon" Canon.tests,
    testGroup "Calculator.Evaluator" Evaluator.tests,
    testGroup "Calculator.Parser" Parser.tests,
    testGroup "Calculator.SynCheck" SynCheck.tests,
    testGroup "Calculator.Functions" Functions.tests
    ]
