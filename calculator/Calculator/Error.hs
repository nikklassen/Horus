module Calculator.Error (
    Safe,
    throwError
) where

import Control.Monad.Except (Except, throwError)

type Safe = Except String
