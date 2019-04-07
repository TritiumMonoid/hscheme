module Main where

import Lexer

main :: IO ()
main = do
  let program = "(begin (define r 10) (* pi (* r r)))"
  putStrLn . show. tokens $ program
