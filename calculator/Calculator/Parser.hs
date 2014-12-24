module Calculator.Parser (
    parse,
    m
) where

import Calculator.Data.AST
import Calculator.Parser.Helpers
import Control.Applicative ((<$>), (*>), (<*))
import Data.Generics.Aliases (extQ)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (mkName)
import Language.Haskell.TH.Lib (ExpQ, PatQ, varE, varP, conP, conE, appE)
import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Prim as Parsec (parse)

numeric :: Parser AST
numeric =
    try (string "$n:" *> (ANum <$> name))
    <|> Number <$> choice [decimal, float]
    <?> "numeric"

function :: Parser AST
function = try $ do
    fName <- name
    suffix <- option [] $ try $
        char '_' *> (flip (:) [] <$> numeric)
    _ <- char '('
    es <- expr `sepBy` char ','
    _ <- char ')'
    return $ FuncExpr fName $ suffix ++ es

identifier :: Parser AST
identifier = try (do
                _ <- char '$'
                choice [ AVar <$> (string "v:" *> name)
                       , AId <$> name <* notFollowedBy (char ':')
                       ])
             <|> Var <$> try name
             <?> "identifier"

eqlStatement :: Parser AST
eqlStatement = do
    lhs <- try $ choice [function, identifier] <* spaces <* char '='
    spaces
    rhs <- expr
    return $ EqlStmt lhs rhs

bindStatement :: Parser AST
bindStatement = do
    lhs <- try $ identifier <* spaces <* string ":="
    spaces
    rhs <- expr
    return $ BindStmt lhs rhs

statement :: Parser AST
statement = (eqlStatement
            <|> bindStatement
            <|> expr) <* eof
            <?> "statement"

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
         <|> function
         <|> identifier
         <|> numeric
         <?> "term"
    spaces
    return t
    where enclosed c1 e c2 = char c1 *> e <* char c2

parse :: String -> AST
parse = getParseResult . Parsec.parse statement ""

m :: QuasiQuoter
m = QuasiQuoter { quoteExp = parseExp
                , quotePat = parsePat
                , quoteType = undefined
                , quoteDec = undefined
                }

parseExp :: String -> ExpQ
parseExp str = do
    l <- fileLocation
    let c = getParseResult $ Parsec.parse (setPosition l *> statement) "" str
    dataToExpQ (const Nothing `extQ` antiE) c

parsePat :: String -> PatQ
parsePat str = do
    l <- fileLocation
    let c = getParseResult $ Parsec.parse (setPosition l *> statement) "" str
    dataToPatQ (const Nothing `extQ` antiP) c

-- Antiquotation replacements
antiE :: AST -> Maybe ExpQ
antiE (AId i) = Just $ varE $ mkName i
antiE (AVar v) = Just $ appE (conE 'Var) (varE $ mkName v)
antiE (ANum n) = Just $ appE (conE 'Number) (varE $ mkName n)
antiE _ = Nothing

antiP :: AST -> Maybe PatQ
antiP (AId i) = Just $ varP $ mkName i
antiP (AVar v) = Just $ conP 'Var [varP $ mkName v]
antiP (ANum n) = Just $ conP 'Number [varP $ mkName n]
antiP _ = Nothing
