module LexicalAnalyzer (tokens) where

import Token
import Text.Regex

regexs :: [(String -> Token, Regex)]
regexs = [ (const Comment, mkRegex ";.*$")
         , (const Comment, mkRegex "\\|\\#(.|\n)*?\\#\\|")
         , (const OpenParenthesis, mkRegex "(")
         , (const CloseParenthesis, mkRegex ")")
         , (const Quote, mkRegex "'|quote")
         , (const Define, mkRegex "define")
         , (const Whitespace, mkRegex "\\s+")
         , (Number, mkRegex "[0-9]+")
         , (Literal, mkRegex "\"((\\\")|[^\"])*\"")
         , (Symbol, mkRegex "[^\\s()\\[\\]{}'\";]+")
         ]

match :: String -> (String -> Token, Regex) -> Maybe (String, Token, String)
match text (token, regex) = match' (matchRegexAll regex text)
  where match' (Just (before, matched, after, _)) =
          Just (before, token matched, after)
        match' _ = Nothing

tokens :: String -> [Token]
tokens text = []
