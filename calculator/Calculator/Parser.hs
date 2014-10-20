{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Calculator.Parser (
    parse
) where

import Calculator.Data.AST
import Calculator.Parser.Helpers
import Control.Applicative ((<$>), (*>), (<*))
import Text.Parsec.Char(char, spaces, space, string)
import Text.Parsec.Combinator (many1, choice, eof, sepBy, option)
import Text.Parsec.Error (errorPos)
import Text.Parsec.Expr
import Text.Parsec.Pos (sourceColumn)
import Text.Parsec.Prim ((<?>), (<|>), try)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Prim as Parsec (parse)

numeric :: Parser AST
numeric = Number <$> choice [decimal, float]

functionArgs :: Parser [AST]
functionArgs = do
    suffix <- option [] $ try $
        char '_' *> (flip (:) [] <$> numeric)
    _ <- char '('
    es <- expr `sepBy` char ','
    _ <- char ')'
    return $ suffix ++ es

varOrFunction :: Parser AST
varOrFunction = do
    f <- identifier
    try $ do
        args <- functionArgs
        return $ FuncExpr f args
        <|> return (Var f)

eqlStatement = do
    lhs <- varOrFunction
    spaces
    _ <- char '='
    spaces
    rhs <- expr
    return $ EqlStmt lhs rhs

bindStatement = do
    lhs <- identifier
    spaces
    _ <- string ":="
    spaces
    rhs <- expr
    return $ BindStmt (Var lhs) rhs

statement :: Parser AST
statement = (try eqlStatement
            <|> try bindStatement
            <|> expr) <* eof
            <?> "expr"

expr :: Parser AST
expr = buildExpressionParser operators term

-- Ordered by precedence
operators = [ [ postfix '\xB0' (FuncExpr "deg" . toArray) ] -- degree sign
            , [ postfix '!' (FuncExpr "!" . toArray) ]
            , [ binary '^' (OpExpr "^") AssocRight ]
            , [ binary '*' (OpExpr "*") AssocLeft
              , binary '/' (OpExpr "/") AssocLeft
              ]
            , [ binary '+' (OpExpr "+") AssocLeft
              , binary '-' (OpExpr "-") AssocLeft
              ]
            , [ binary '%' (OpExpr "%") AssocLeft ]
            ]
            where binary op func = Infix (char op >> return func)
                  postfix op func = Postfix (char op >> return func)
                  toArray = flip (:) []

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
            Left err -> error $ "at position " ++ show (sourceColumn $ errorPos err)
            Right ts -> ts
