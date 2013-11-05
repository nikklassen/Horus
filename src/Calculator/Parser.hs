module Calculator.Parser(
    parse
) where

import Calculator.Scanner

fixNegs :: [Token] -> Either String [Token]
fixNegs tokens = fixNegsAcc tokens []

fixNegsAcc :: [Token] -> [Token] -> Either String [Token]
fixNegsAcc [] acc = Right acc
fixNegsAcc (t:ts) acc
    | (getKind t) `elem` [Lparen, Op] =
        if null ts then
           Left $ "ERROR: Incomplete expression"
        else
           let next = head ts
           in if (getLex next) == "-" then
                 if length ts < 2 then
                    Left "ERROR: Incomplete expression"
                 else let value = (ts !! 1)
                      in if (getKind value) /= Numeric then
                           Left $ "Invalid token " ++ (getLex value)
                         else fixNegsAcc (drop 2 ts) $ t : (Token Numeric $ '-' : getLex value) : acc
           else fixNegsAcc ts (t:acc)
    | otherwise = fixNegsAcc ts (t:acc)

parse :: [Token] -> Either String [Token]
parse tokens = parseAcc tokens [] []

parseAcc :: [Token] -> [Token] -> [Token] -> Either String [Token]
parseAcc [] output [] = Right output
parseAcc [] output opStack@(op:ops)
    | getKind op == Lparen = Left "ERROR: mismatch brackets"
    | otherwise = parseAcc [] (op:output) ops
parseAcc tokens@(t:ts) output opStack@(op:ops)
    | kind `elem` [Numeric, Id] = parseAcc ts (t:output) opStack
    | kind == Op =
        if null opStack || precedence t > (precedence $ head opStack) then
            parseAcc ts output $ t:opStack
        else
            parseAcc tokens ((head opStack):output) $ tail opStack
    | kind == Lparen = parseAcc ts output $ t:opStack
    | kind == Rparen =
        if null opStack then
            Left "ERROR: mismatched brackets"
        else
            let op = head opStack
            in if (getKind op) == Lparen then
                parseAcc ts output $ tail opStack
            else
                parseAcc tokens (op:output) $ tail opStack
    where kind = getKind t

precedence :: Token -> Int
precedence t = case getLex t of
    "%" -> 0
    "+" -> 1
    "-" -> 1
    "*" -> 2
    "/" -> 2
    "^" -> 3
