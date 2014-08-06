{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.Parser.Helpers (
    identifier,
    decimal,
    float
) where

import Control.Applicative ((<$>))
import Data.Char
import Data.Number.CReal
import Text.Parsec.Char (char, oneOf, digit, letter, alphaNum)
import Text.Parsec.Combinator (many1, option)
import Text.Parsec.Prim (many, (<|>), try)
import Text.Parsec.String (Parser)

power :: Parser CReal
power = do
    _ <- oneOf "eE"
    s <- sign
    n <- number
    return $ 10 ** s n
    where sign = (char '-' >> return negate) <|> return id

number :: Parser CReal
number = foldl (\n d -> n * 10 + toInt d) 0 <$> many1 digit
         where toInt c = fromIntegral $ ord c - ord '0'

postDec :: Parser CReal
postDec = foldr (\d n -> (n + toInt d)/10.0) 0.0 <$> many1 digit
          where toInt :: Char -> CReal
                toInt c = fromIntegral $ ord c - ord '0'

decimal :: Parser CReal
decimal = do
    _ <- char '.'
    n <- postDec
    p <- option 1 power
    return $ n * p

float :: Parser CReal
float = do
    n <- number
    d <- option 0 $ try $ do
            _ <- char '.'
            postDec
    p <- option 1 power
    return $ (n + d) * p

identifier :: Parser String
identifier = do
    h <- letter
    e <- many alphaNum
    return $ h:e
