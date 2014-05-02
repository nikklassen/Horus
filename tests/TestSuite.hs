module Main where

import Test.Framework(defaultMain, testGroup)
import qualified Calculator.Scanner.Tests as Scanner
import qualified Calculator.Parser.Tests as Parser
import qualified Calculator.Tests as Calculator

main = defaultMain [
    testGroup "Calculator.Scanner.Tests" Scanner.tests,
    testGroup "Calculator.Parser.Tests" Parser.tests,
    testGroup "Calculator.Tests" Calculator.tests
    ]
