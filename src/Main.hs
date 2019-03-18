module Main where

import Data.Maybe
import qualified Data.Text as T
import qualified Text.Regex as R

data Sexp = Sexp [Sexp] | Symbol String | Number String
instance Show Sexp where
  show (Sexp sexps) = show sexps
  show (Symbol symbol) = show symbol
  show (Number number) = show number

tokenize :: String -> [String]
tokenize program =
  map T.unpack
  $ filter (\token -> not (token == (T.pack " ") || token == (T.pack "")))
  $ T.splitOn (T.pack " ")
  $ T.replace (T.pack ")") (T.pack " ) ")
  $ T.replace (T.pack "(") (T.pack " ( ")
  $ T.pack program

isNumber :: String -> Bool
isNumber = isJust . R.matchRegex (R.mkRegex "^[0-9]+$")

isSymbol :: String -> Bool
isSymbol = isJust . R.matchRegex (R.mkRegex "^.+$")

parse :: [String] -> Sexp
parse tokens = Sexp (snd (parseSexp tokens []))

parseSexp :: [String] -> [Sexp] -> ([String], [Sexp])
parseSexp [] [] = ([], [Symbol "unexpected EOF"])
parseSexp [] sexps = ([], sexps)
parseSexp (token:tokens) sexps
  | token == "(" =
    let (tokens', sexps') = parseSexp tokens []
    in parseSexp tokens' (sexps ++ [Sexp sexps'])
  | token == ")" = (tokens, sexps)
  | isSymbol token = parseSexp tokens (sexps ++ [Symbol token])
  | isNumber token = parseSexp tokens (sexps ++ [Number token])
  | otherwise = parseSexp tokens sexps

main :: IO ()
main = putStrLn . show . parse . tokenize $ "(begin (define r 10) (* pi (* r r)))"
