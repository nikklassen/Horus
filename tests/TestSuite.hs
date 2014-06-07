module Main where

import Test.Framework(defaultMain, testGroup)
import qualified Calculator.Parser.Tests as Parser
import qualified Calculator.Tests as Calculator

main = defaultMain [
    testGroup "Calculator.Parser.Tests" Parser.tests,
    testGroup "Calculator.Tests" Calculator.tests
    ]
