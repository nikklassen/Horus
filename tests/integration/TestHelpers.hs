module TestHelpers where

import Control.DeepSeq (($!!), NFData)
import Control.Exception (ErrorCall(..))
import Test.HUnit
import Test.HUnit.Tools
import qualified Control.Exception.Lifted as CEL

infix 1 @?==
(@?==) :: (Eq a, Show a, NFData a) => a ->        -- actual value
                                      a ->        -- expected value
                                      Assertion
actual @?== expected = (CEL.evaluate $!! actual) >>= (@?= expected)

deepAssertRaises :: (Show a, NFData a) => String ->
                                          a ->
                                          Assertion
deepAssertRaises msg expr = assertRaises "" (ErrorCall msg)
                                            (CEL.evaluate $!! expr)
