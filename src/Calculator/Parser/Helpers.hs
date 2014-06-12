{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.Parser.Helpers (
    decimal,
    float,
    identifier
) where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, oneOf, string, digit, letter, alphaNum)
import Text.Parsec.Combinator (many1, option)
import Control.Applicative (Applicative(..), (<$>), (<*>))
import Text.Parsec.Prim (many)

(<:>) a b = (:) <$> a <*> b
(<++>) a b = (++) <$> a <*> b

power :: Parser String
power = option "" $ oneOf "eE" <:> signed
        where signed = option "" (string "-") <++> number

number :: Parser String
number = many1 digit

decimal :: Parser String
decimal = (char '.' <:> number) <++> option "" power

float :: Parser String
float = number <++> option "" decimal <++> option "" power

identifier :: Parser String
identifier = do
    h <- letter
    e <- many alphaNum
    return $ h:e
