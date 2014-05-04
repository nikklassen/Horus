module Calculator where

import qualified Calculator.Scanner as S
import qualified Calculator.Parser as P
import Calculator.Data.Token
import Data.Number.CReal
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Result = Result {
    answer :: Maybe CReal,
    vars :: Map String CReal
} deriving (Eq, Show)

calculate :: String -> Map String CReal -> Result
calculate "" variables = Result Nothing variables
calculate expression variables =
    let (lhs, rhs) = P.getEquation $ P.fixNegs $ S.scan expression
        result = evaluate (P.parse rhs) variables
    in Result result $ addToVars lhs result variables

addToVars :: [Token] -> Maybe CReal -> Map String CReal -> Map String CReal
addToVars _ Nothing _ = error "ERROR: Expected rhs"
addToVars lhs (Just result) variables
    | null lhs = variables
    | length lhs == 1 =
        let token = head lhs
        in if getKind token /= Id then
            error "ERROR: Expected id on lhs of expression"
        else
            Map.alter (\_ -> Just result) (getLex token) variables
    | otherwise = error "ERROR: Expected single id on lhs of expression"

evaluate :: [Token] -> Map String CReal -> Maybe CReal
evaluate tokens variables = listToMaybe $ foldl (performOperation $ variables) [] tokens

performOperation :: Map String CReal -> [CReal] -> Token -> [CReal]
performOperation variables stack token
    | kind == Function =
        if null stack then
            error $ "Missing arguments for lex " ++ lexeme
        else
            let (s1:ss) = stack
            in  ((fromJust $ Map.lookup lexeme functions) s1):ss
    | kind == Op =
        if length stack < 2 then
            error $ "Missing arguments for operation " ++ lexeme
        else
            let (s1:s2:ss) = stack
            in (operate lexeme s1 s2):ss
    | kind == Numeric =
        (toNumber lexeme):stack
    | kind == Id =
        let val = case Map.lookup lexeme variables of
                      Just v -> v
                      Nothing -> error $ "ERROR: Use of undefined variable " ++ lexeme
        in val:stack
    | otherwise = error $ "Invalid operation " ++ lexeme
    where kind = getKind token
          lexeme = getLex token
        
isFunction :: String -> Bool
isFunction = flip Map.member functions

functions = Map.fromList
    [ ("sin", sin)
    , ("cos", cos)
    , ("tan", tan)
    , ("asin", asin)
    , ("acos", acos)
    , ("atan", atan)
    , ("neg", negate)
    ]

toNumber :: String -> CReal
toNumber num
    | 'e' `elem` num =
        let (a, b) = break (=='e') num
            base = read a
            ex = read $ tail b
        in base * 10**ex
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

realMod :: CReal -> CReal -> CReal
realMod a b = a - (fromInteger $ floor $ a/b) * b
