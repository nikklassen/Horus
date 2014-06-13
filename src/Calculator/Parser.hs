{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.Parser (
    parse
) where

import Text.Parsec.Char(char, spaces, space)
import Text.Parsec.Prim ((<?>), (<|>), try)
import qualified Text.Parsec.Prim as Parsec (parse)
import Text.Parsec.Combinator (many1, choice, eof, sepBy)
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>), (*>), (<*))
import Text.Parsec.Expr

import Calculator.Data.AST
import Calculator.Parser.Helpers

numeric :: Parser AST
numeric = Number <$> choice [decimal, float]

varOrFunction :: Parser AST
varOrFunction = do
    f <- identifier
    try $ do
        _ <- char '('
        es <- expr `sepBy` char ','
        _ <- char ')'
        return $ FuncExpr f es
        <|> return (Var f)

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
            where binary op func = Infix (char op >> return func)

term :: Parser AST
term = do
    t <- (many1 space >> term)
         <|> (char '-' >> Neg <$> term)
         <|> enclosed '(' expr ')'
         <|> enclosed '[' expr ']'
         <|> varOrFunction
         <|> numeric
         <?> "term"
    spaces
    return t
    where enclosed c1 e c2 = char c1 *> e <* char c2

parse :: String -> AST
parse s = case Parsec.parse statement "" s of
            Left err -> error $ "ERROR: " ++ show err
            Right ts -> ts
