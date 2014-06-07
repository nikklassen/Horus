module Calculator.Parser (
    parse
) where

import Text.Parsec.Char
import Text.Parsec.Prim (many, (<?>), (<|>), try)
import qualified Text.Parsec.Prim as Parsec (parse)
import Text.Parsec.Combinator (many1, option, choice, eof)
import Text.Parsec.String
import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Text.Parsec.Expr

import Calculator.Data.AST (AST(..))
import qualified Calculator.Data.AST as AST
import Calculator.Functions

(<:>) a b = (:) <$> a <*> b
(<++>) a b = (++) <$> a <*> b

-- Helper Parsers
number :: Parser String
number = many1 digit

power :: Parser String
power = option "" $ oneOf "eE" <:> signed
        where signed = (option "" (string "-")) <++> number

decimal :: Parser String
decimal = (char '.' <:> number) <++> (option "" power)

float :: Parser String
float = number <++> (option "" decimal) <++> (option "" power)

identifier :: Parser String
identifier = do
    h <- letter
    e <- many alphaNum
    return $ h:e

-- Grammar
numeric :: Parser AST
numeric = AST.toNumber <$> choice [decimal, float]

varOrFunction :: Parser AST
varOrFunction = do
    f <- identifier
    if isFunction f then
        do
            _ <- char '(' 
            e <- expr
            _ <- char ')'
            return $ Function f e
        else
            return $ Var f

statement :: Parser AST
statement = (try (do
                i <- identifier
                spaces
                _ <- char '='
                spaces
                e <- expr
                return $ EqlStmt (Var i) e)
            <|> expr) <* eof
            <?> "expr"

expr :: Parser AST
expr = buildExpressionParser operators term

operators = [ [ binary '^' (OpExpr "^") AssocRight ]
            , [ binary '*' (OpExpr "*") AssocLeft
              , binary '/' (OpExpr "/") AssocLeft
              ]
            , [ binary '+' (OpExpr "+") AssocLeft
              , binary '-' (OpExpr "-") AssocLeft
              ]
            , [ binary '%' (OpExpr "%") AssocLeft ]
            ]
            where binary op func assoc = Infix (char op >> return func) assoc

term :: Parser AST
term = do
    t <- ((many1 space >> term)
         <|> (char '-' >> Neg <$> term)
         <|> enclosed '(' expr ')'
         <|> enclosed '[' expr ']'
         <|> varOrFunction
         <|> numeric
         <?> "term")
    spaces
    return t
    where enclosed c1 e c2 = char c1 *> e <* char c2

parse :: String -> AST
parse s = case Parsec.parse statement "" s of
            Left err -> error $ "ERROR: " ++ (show err)
            Right ts -> ts
