module Calculator.Lexer (
    scan
) where

import Calculator.Data.Token
import Text.Parsec.Char
import Text.Parsec.Prim (many, parse, (<?>))
import Text.Parsec.Combinator (many1, option, choice, notFollowedBy, sepEndBy)
import Text.Parsec.String
import Control.Applicative ((<$>), (<*>))
import Calculator.Functions

(<:>) a b = (:) <$> a <*> b
(<++>) a b = (++) <$> a <*> b

-- Helper Parsers
charToString :: Char -> String
charToString c = [c]

number :: Parser String
number = many1 digit

power :: Parser String
power = option "" $ oneOf "eE" <:> signed
        where signed = (option "" (string "-")) <++> number

decimal :: Parser String
decimal = (char '.' <:> number) <++> (option "" power)

float :: Parser String
float = number <++> (option "" decimal) <++> (option "" power)

-- Token Parsers

numeric :: Parser Token
numeric = (Token Numeric) <$> choice [decimal, float]

identifier :: Parser Token
identifier = do
    h <- letter
    e <- many alphaNum
    let lexeme = (h : e)
    return $ if isFunction lexeme then
        Token Function lexeme
        else
            Token Id lexeme

-- Prevents numbers from being followed by an id
-- or ids from being followed by a numeric other than "number"
numericOrIdentifier :: Parser Token
numericOrIdentifier = do
                        a <- choice [identifier, numeric]
                        notFollowedBy (choice [identifier, numeric]) <?> "whitespace"
                        return a

lParen :: Parser Token
lParen = (Token Lparen . charToString) <$> char '('

rParen :: Parser Token
rParen = (Token Rparen . charToString) <$> char ')'

equals :: Parser Token
equals = (Token Eql . charToString) <$> char '='

operator :: Parser Token
operator = (Token Op . charToString) <$> oneOf "-+/*%^"
                                     <?> "operator"
 
-- Master token parser
tokens :: Parser [Token]
tokens = do
    spaces
    sepEndBy (choice [ equals
                          , lParen
                          , rParen
                          , numericOrIdentifier -- this has to be before operator to give binary operators precedence
                          , operator
                          ])
                  spaces

scan :: String -> [Token]
scan s = case parse tokens "" s of
            Left err -> error $ "ERROR: " ++ (show err)
            Right ts -> ts
