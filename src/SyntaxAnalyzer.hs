module SyntaxAnalyzer (parse) where

import Token

data Expr = Expr [Expr] | Atom Token

instance Show Expr where
  show (Expr exprs) = show exprs
  show (Atom (Symbol symbol)) = show symbol
  show (Atom (Number number)) = show number
  show (Atom other) = show other

parseExpr :: [Token] -> [Expr] -> ([Token], [Expr])
parseExpr [] [] = ([], [Atom (Symbol "unexpected EOF")])
parseExpr [] exprs = ([], exprs)
parseExpr (token:tokens) exprs = case token of
  OpenParenthesis ->
    let (tokens', exprs') = parseExpr tokens []
    in parseExpr tokens' (exprs ++ [Expr exprs'])
  CloseParenthesis -> (tokens, exprs)
  Symbol _ -> parseExpr tokens (exprs ++ [Atom token])
  Number _ -> parseExpr tokens (exprs ++ [Atom token])
  _ -> parseExpr tokens exprs

parse :: [Token] -> Expr
parse tokens = Expr (snd (parseExpr tokens []))
