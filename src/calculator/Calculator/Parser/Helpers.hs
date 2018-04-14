module Calculator.Parser.Helpers (
    name,
    decimal,
    float,
    fileLocation,
    getParseResult
) where

import Calculator.Data.Decimal
import Data.Char
import Language.Haskell.TH.Syntax
import Text.Parsec.Char (char, oneOf, digit, letter, alphaNum)
import Text.Parsec.Combinator (many1, option)
import Text.Parsec.Error (errorPos, ParseError)
import Text.Parsec.Prim (many, (<|>), try)
import Text.Parsec.Pos (SourcePos, newPos, sourceColumn)
import Text.Parsec.String (Parser)

power :: Parser Decimal
power = do
    _ <- oneOf "eE"
    s <- sign
    n <- number
    return $ 10 ** s n
    where sign = (char '-' >> return negate) <|> return id

number ::Parser Decimal
number = foldl (\n d -> n * 10 + toInt d) 0 <$> many1 digit
         where toInt c = fromIntegral $ ord c - ord '0'

postDec ::Parser Decimal
postDec = foldr (\d n -> (n + toInt d)/10.0) 0.0 <$> many1 digit
          where toInt c = fromIntegral $ ord c - ord '0'

decimal ::Parser Decimal
decimal = do
    _ <- char '.'
    n <- postDec
    p <- option 1 power
    return $ n * p

float :: Parser Decimal
float = do
    n <- number
    d <- option 0 $ try $ do
            _ <- char '.'
            postDec
    p <- option 1 power
    return $ (n + d) * p

name :: Parser String
name = do
    h <- letter
    e <- many alphaNum
    return $ h:e

getParseResult :: Either ParseError a -> a
getParseResult (Left err) = error $ "at position " ++ show (sourceColumn $ errorPos err)
getParseResult (Right a) = a

-- Quasi quoter helpers
fileLocation :: Q SourcePos
fileLocation = aux <$> location
               where aux :: Loc -> SourcePos
                     aux loc = uncurry (newPos (loc_filename loc))
                                       (loc_start loc)
