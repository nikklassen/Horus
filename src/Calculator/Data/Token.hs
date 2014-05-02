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
          | Eql
          deriving(Eq,Show)

data Token = Token {
    getKind :: Kind,
    getLex :: String
} deriving( Eq, Show )
