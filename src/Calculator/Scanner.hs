module Calculator.Scanner (
    scan,
    Token(..)
) where

import Data.Char(isAlpha,isDigit,isSpace)
import Data.List
import Data.Maybe
import Control.Applicative()
import Control.Exception()
import Calculator.Data.Token

data State = ST_START
           | ST_INT
           | ST_WS
           | ST_DEC
           | ST_E
           | ST_EXP
           | ST_OP
           | ST_ID
           | ST_LPAREN
           | ST_RPAREN
           | ST_EQL
           | ST_NULL
           deriving(Eq,Show)

data Transition = Transition {
    getState :: State,
    getCharSet :: Char -> Bool,
    getToState :: State
}

scan :: String -> [Token]
scan str = scanAcc str transitionTable ST_START finalStates "" []

scanAcc :: String -> [Transition] -> State -> [State] -> String -> [Token] -> [Token]
scanAcc [] _ cState fStates lexAcc tokAcc
    | cState `elem` fStates =
        reverse $
        if cState == ST_WS then
            tokAcc
        else
            makeToken cState (reverse lexAcc) : tokAcc
    | otherwise = error $ "ERROR: Ended in unacceptable state " ++ show cState

scanAcc str@(chr:chrs) trTable cState fStates lexAcc tokAcc =
    let
        foundTrans state c t = state == getState t && getCharSet t c
        tr = find (foundTrans cState chr) trTable
    in if isNothing tr then
        if cState `elem` fStates then
            scanAcc str trTable ST_START fStates [] (
                if cState == ST_WS then
                    tokAcc
                else
                    makeToken cState (reverse lexAcc) : tokAcc
            )
        else
            error $ "ERROR: Left to parse " ++ show cState ++ " " ++ str
    else
        let
            toState = getToState $ fromJust tr
        in if toState == ST_NULL then
            error $ "ERROR: Invalid symbol " ++ [chr]
        else
            scanAcc chrs trTable toState fStates (chr:lexAcc) tokAcc

makeToken :: State -> String -> Token
makeToken state lexeme
    | kind == Numeric && head lexeme == '.' = Token kind ('0':lexeme)
    | kind == Id =
        if lexeme `elem` ["sin","cos","tan","asin","acos","atan"] then
           Token Function lexeme
        else
            Token Id lexeme
    | otherwise = Token kind lexeme
    where kind = stateToKind state

stateToKind :: State -> Kind
stateToKind state
    | state `elem` [ST_INT, ST_DEC, ST_EXP] = Numeric
    | state == ST_OP = Op
    | state == ST_ID = Id
    | state == ST_LPAREN = Lparen
    | state == ST_RPAREN = Rparen
    | state == ST_EQL = Eql

isOperator :: Char -> Bool
isOperator = flip elem "+-/*^%"

notMinus :: Char -> Bool
notMinus x = isOperator x && x /= '-'

notE :: Char -> Bool
notE x = isAlpha x && x /= 'e'

transitionTable :: [Transition]
transitionTable = 
    [ Transition ST_START isDigit ST_INT
    , Transition ST_START isAlpha ST_ID
    , Transition ST_START isSpace ST_WS
    , Transition ST_START isOperator ST_OP
    , Transition ST_START (== '(') ST_LPAREN
    , Transition ST_START (== ')') ST_RPAREN
    , Transition ST_START (== '.') ST_DEC
    , Transition ST_START (== '=') ST_EQL
    , Transition ST_INT isDigit ST_INT
    , Transition ST_INT (== '.') ST_DEC
    , Transition ST_INT (== 'e') ST_E
    , Transition ST_E (\x -> x == '-' || isDigit x) ST_EXP
    , Transition ST_INT notE ST_NULL
    , Transition ST_DEC isDigit ST_DEC
    , Transition ST_DEC (== '.') ST_NULL
    , Transition ST_DEC (== 'e') ST_EXP
    , Transition ST_DEC notE ST_NULL
    , Transition ST_EXP isDigit ST_EXP
    , Transition ST_EXP (== '.') ST_NULL
    , Transition ST_EXP isAlpha ST_NULL
    , Transition ST_ID isAlpha ST_ID
    , Transition ST_ID isDigit ST_NULL
    , Transition ST_ID (== '.') ST_NULL
    , Transition ST_OP notMinus ST_NULL
    ]

finalStates :: [State]
finalStates =
    [ ST_INT
    , ST_WS
    , ST_DEC
    , ST_EXP
    , ST_OP
    , ST_ID
    , ST_LPAREN
    , ST_RPAREN
    , ST_EQL
    ]
