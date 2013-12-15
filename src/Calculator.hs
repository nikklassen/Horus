module Calculator where

import qualified Calculator.Scanner as S
import qualified Calculator.Parser as P
import Calculator.Data.Token
import Data.Number.CReal
import qualified Data.Map as Map
import Data.Maybe
import Data.Fixed

main = putStrLn "Hi"

calculate :: String -> CReal
calculate expression =
    let tokens = P.parse $ P.fixNegs $ S.scan expression
    in evaluate tokens

evaluate :: [Token] -> CReal
evaluate tokens = head $ foldl performOperation [] tokens

performOperation :: [CReal] -> Token -> [CReal]
performOperation stack token
    | kind == Function =
        if null stack then
            error "Missing arguments for " ++ lexeme
        else
            ((fromJust $ Map.lookup lexeme functions) s1):s2:ss
    | kind == Op =
        (operate lexeme s1 s2):ss
    | otherwise =
        (toNumber lexeme):stack
    where kind = getKind token
          lexeme = getLex token
-- evaluate stack@(s1:ss) symbol

{--
isFunction :: String -> Bool
isFunction = flip elem (Map.keys functions)
--}

toNumber :: String -> CReal
toNumber num
    | 'e' `elem` num =
        let (a, b) = break (=='e') num
            base = read a
            ex = read $ tail b
        in base**ex
    | otherwise = read num

operate :: String -> CReal -> CReal -> CReal
operate op n1 n2 =
    case op of
        "+" -> n1 + n2
        "*" -> n1 * n2
        "-" -> n2 - n1
        "/" -> n2 / n1
        "^" -> n2**n1
        "%" -> realMod n2 n1

realMod :: (Real a) => a -> a -> a
realMod a b = a `mod'` b

functions = Map.fromList
    [ ("sin", sin)
    , ("cos", cos)
    , ("tan", tan)
    , ("asin", asin)
    , ("acos", acos)
    , ("atan", atan)
    , ("neg", negate)
    ]
