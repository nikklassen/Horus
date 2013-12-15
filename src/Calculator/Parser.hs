module Calculator.Parser(
    fixNegs,
    parse
) where

import Calculator.Data.Token
import Data.Maybe

fixNegs :: [Token] -> [Token]
fixNegs tokens = fixNegsAcc tokens [] Nothing

fixNegsAcc :: [Token] -> [Token] -> Maybe Token -> [Token]
fixNegsAcc [] acc _ = reverse acc
fixNegsAcc (t:ts) acc prev
    | isNothing prev =
        if getKind t == Op && getLex t == "-" then
            fixNegsAcc ts (negToken:acc) $ Just negToken
        else
            fixNegsAcc ts (t:acc) $ Just t
    | otherwise =
        if (getKind t == Op && getLex t == "-") && (prevKind `elem` [Function, Op, Lparen]) then
              fixNegsAcc ts (negToken:acc) $ Just negToken
        else
            fixNegsAcc ts (t:acc) $ Just t
   where prevKind = getKind $ fromJust prev
         negToken = Token Function "neg"        

parse :: [Token] -> [Token]
parse tokens = parseAcc tokens [] []

parseAcc :: [Token] -> [Token] -> [Token] -> [Token]
parseAcc [] output [] = reverse output
parseAcc [] output (op:ops)
    | getKind op == Lparen = error "ERROR: mismatched brackets"
    | otherwise = parseAcc [] (op:output) ops
parseAcc tokens@(t:ts) output opStack
    | kind `elem` [Numeric, Id] = parseAcc ts (t:output) opStack
    | kind `elem` [Op, Function] =
        if null opStack || getKind (head opStack) == Lparen || precedence t > precedence (head opStack) then
            parseAcc ts output $ t:opStack
        else
            parseAcc tokens (head opStack : output) $ tail opStack
    | kind == Lparen = parseAcc ts output (t:opStack)
    | kind == Rparen =
        if null opStack then
            error "ERROR: mismatched brackets"
        else
            let op = head opStack
            in if getKind op == Lparen then
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
    _ -> 4 -- functions
