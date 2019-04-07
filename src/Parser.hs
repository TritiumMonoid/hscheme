module Parser (parse) where

import Token
import Control.Monad.State

data Expr = Expr [Expr] | ESymbol String | ENumber String

instance Show Expr where
  show (Expr exprs) = show exprs
  show (ESymbol symbol) = show symbol
  show (ENumber number) = show number

parseExpr :: [Token] -> [Expr] -> ([Token], [Expr])
parseExpr [] [] = ([], [ESymbol "unexpected EOF"])
parseExpr [] exprs = ([], exprs)
parseExpr (token:tokens) exprs = case token of
  OpenParenthesis ->
    let (tokens', exprs') = parseExpr tokens []
    in parseExpr tokens' (exprs ++ [Expr exprs'])
  CloseParenthesis -> (tokens, exprs)
  Symbol _ -> parseExpr tokens (exprs ++ [ESymbol token])
  Number _ -> parseExpr tokens (exprs ++ [ENumber token])
  _ -> parseExpr tokens exprs

parse :: [Token] -> Expr
parse tokens = Expr (snd (parseExpr tokens []))
