module Calculator.Tests (
    tests
) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Control.Exception(ErrorCall(..))
import Calculator -- hiding (evaluate)
import Data.Map
import Test.HUnit.Tools
import Control.DeepSeq
import Data.Number.CReal

instance Eq ErrorCall where
    x == y = show x == show y

instance NFData CReal

instance NFData Result where
    rnf (Result a m) = rnf a `seq` rnf m

forceM :: (Monad m, NFData a) => m a -> m a
forceM m = m >>= (return $!) . force

tests = []
{--
tests = [ testGroup "Numbers" [
            testCase "Exponential" parseExp
            , testCase "Negative Exponential" parseNegExp
            , testCase "Double" parseDouble
            ]
        , testGroup "Operations" [
            testCase "Plus op" plusOp
            , testCase "Minus op" minusOp
            , testCase "Mult op" multOp
            , testCase "Div op" divOp
            , testCase "Exp op" expOp
            , testCase "Mod op" modOp
            ]
        , testGroup "Calculate" [
            testCase "Binary operations" calc_binary
            , testCase "Order of operations" calc_bedmas
            , testCase "Parentheses" calc_parens
            , testCase "Functions" calc_func
            , testCase "Variable substition" calc_vars
            , testCase "Variable assignment" calc_add_var
            , testCase "Replace existing var" calc_replace_var
            , testCase "Empty" calc_empty
            ]
        , testGroup "Calculate error" [
            testCase "Assign to int" calc_assign_to_int
            , testCase "Missing rhs" calc_no_rhs
            , testCase "Expr on lhs" calc_big_lhs
            , testCase "Use undefined var" calc_undefined_var
            ]
        ]

parseExp = toNumber "3e2" @?= 300
parseNegExp = toNumber "5e-1" @?= 0.5
parseDouble = toNumber "10.23" @?= 10.23

plusOp = operate "+" 1 2 @?= 3
minusOp = operate "-" 5 2 @?= -3
multOp = operate "*" 4 2 @?= 8
divOp = operate "/" 2 5 @?= 2.5
expOp = operate "^" 1.5 4 @?= 8
modOp = operate "%" 1 1.5 @?= 0.5

emptyMap = fromList []

calc_binary = calculate "4 + 5 - 9" emptyMap @?= Result (Just 0) emptyMap

calc_bedmas = calculate "6 * 2 + 10 / 5 - 2^2 % 3" emptyMap @?= Result (Just 1) emptyMap

calc_parens = calculate "6 * (2 + 10) / (5 - 2)^(2 % 3)" emptyMap @?= Result (Just 8) emptyMap

calc_func = calculate "sin 1" emptyMap @?= Result (Just $ sin 1) emptyMap

calc_empty = calculate "" emptyMap @?= Result Nothing emptyMap

calc_vars = calculate "a + 3" varMap @?= Result (Just 5) varMap
            where varMap = fromList [("a", 2)]

calc_add_var = calculate "b = 3" emptyMap @?= Result (Just 3) (fromList [("b", 3)])

calc_replace_var = calculate "b = b + 3" (fromList [("b", 3)]) @?= Result (Just 6) (fromList [("b", 6)])

calc_assign_to_int = assertRaises "" (ErrorCall "ERROR: Expected id on lhs of expression") (forceM $ return $ calculate "2 = 3" emptyMap)

calc_no_rhs = assertRaises "" (ErrorCall "ERROR: Expected rhs") (forceM $ return $ calculate "a =" emptyMap)

calc_big_lhs = assertRaises "" (ErrorCall "ERROR: Expected single id on lhs of expression") (forceM $ return $ calculate "a b = 2" emptyMap)

calc_undefined_var = assertRaises "" (ErrorCall "ERROR: Use of undefined variable b") (forceM $ return $ calculate "a = b" emptyMap)
--}
