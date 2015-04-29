module Calculator.ASTPass.ConstExpansion (
    constExpansionPass
) where

import Calculator.Data.AST
import Calculator.Error (Safe)

constExpansionPass :: AST -> Safe AST
constExpansionPass = astMap (return . findConsts)
                     where findConsts :: AST -> AST
                           findConsts ast@(Var v) =
                                   case v of
                                       "e" -> Number $ exp 1
                                       "pi" -> Number pi
                                       _ -> ast
                           findConsts ast = ast
