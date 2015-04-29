module TestHelpers where

import Control.Monad.Except (Except, throwError)
import Test.HUnit ((@?=), Assertion)

assertThrows :: (Eq a, Eq e, Show a, Show e) => e -> Except e a -> Assertion
assertThrows err test = test @?= throwError err

infix 1 @?==
(@?==) :: (Eq a, Eq e, Show a, Show e) => Except e a -> a -> Assertion
(@?==) test result = test @?= return result
