module Calculator.Scanner(
    scan,
    Kind(..),
    Token(..)
) where

import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Exception

data Kind = Op
          | Numeric
          | Id
          | Lparen
          | Rparen
          deriving(Eq,Show)

data State = ST_START
           | ST_INT
           | ST_WS
           | ST_DEC
           | ST_EXP
           | ST_OP
           | ST_ID
           | ST_LPAREN
           | ST_RPAREN
           | ST_NULL
           deriving(Eq,Show)

data Transition = Transition {
    getState :: State,
    getCharSet :: Char -> Bool,
    getToState :: State
}

data Token = Token {
    getKind :: Kind,
    getLex :: String
} deriving( Eq, Show )

scan :: String -> Either String [Token]
scan str = scanAcc str transitionTable ST_START finalStates "" []

scanAcc :: String -> [Transition] -> State -> [State] -> String -> [Token] -> Either String [Token]
scanAcc [] _ cState fStates lexAcc tokAcc
    | cState `elem` fStates =
        Right ( reverse $
            if cState == ST_WS then
                tokAcc
            else
                Token (stateToKind cState) (reverse lexAcc) : tokAcc
        )
    | otherwise = Left "ERROR: unexpected end of string\n"

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
                    Token (stateToKind cState) (reverse lexAcc) : tokAcc
            )
        else
            Left $ "ERROR: Left to parse " ++ show cState ++ " " ++ str
    else
        let
            toState = getToState $ fromJust tr
        in if toState == ST_NULL then
            Left $ "ERROR: Invalid symbol " ++ [chr]
        else
            scanAcc chrs trTable toState fStates (chr:lexAcc) tokAcc

stateToKind :: State -> Kind
stateToKind state
    | state `elem` [ST_INT, ST_DEC, ST_EXP] = Numeric
    | state == ST_OP = Op
    | state == ST_ID = Id
    | state == ST_LPAREN = Lparen
    | state == ST_RPAREN = Rparen

isOperator = flip elem "+-/*^%"
notE x = isAlpha x && x /= 'e'

transitionTable = 
    [ Transition ST_START isDigit ST_INT
    , Transition ST_START isAlpha ST_ID
    , Transition ST_START isSpace ST_WS
    , Transition ST_START isOperator ST_OP
    , Transition ST_START (== '(') ST_LPAREN
    , Transition ST_START (== ')') ST_RPAREN
    , Transition ST_START (== '.') ST_NULL
    , Transition ST_INT isDigit ST_INT
    , Transition ST_INT (== '.') ST_DEC
    , Transition ST_INT (== 'e') ST_EXP
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
    , Transition ST_OP isOperator ST_NULL
    ]

finalStates =
    [ ST_INT
    , ST_WS
    , ST_DEC
    , ST_EXP
    , ST_OP
    , ST_ID
    , ST_LPAREN
    , ST_RPAREN
    ]
