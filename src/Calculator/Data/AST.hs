module Calculator.Data.AST (
    AST(..),
    toNumber
) where

import Data.Number.CReal

data AST = EqlStmt AST AST
           | OpExpr String AST AST
           | Function String AST
           | Var String
           | Number CReal
           | Neg AST
           deriving (Show, Eq)

toNumber :: String -> AST
toNumber ('.':xs) = toNumber $ "0." ++ xs
toNumber num
    | 'e' `elem` num =
        let (a, b) = break (=='e') num
            base = read a
            ex = read $ tail b
        in Number $ base * 10**ex
    | otherwise = Number $ read num
