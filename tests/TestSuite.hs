module Main where

import Test.Framework(defaultMain, testGroup)
import qualified Calculator.Scanner.Tests as Scanner
import qualified Calculator.Parser.Tests as Parser

main = defaultMain [
    testGroup "Calculator.Scanner.Tests" Scanner.tests,
    testGroup "Calculator.Parser.Tests" Parser.tests
    ]
