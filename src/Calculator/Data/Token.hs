module Calculator.Data.Token(
    Token(..),
    Kind(..)
) where

data Kind = Op
          | Numeric
          | Id
          | Function
          | Lparen
          | Rparen
          deriving(Eq,Show)

data Token = Token {
    getKind :: Kind,
    getLex :: String
} deriving( Eq, Show )
