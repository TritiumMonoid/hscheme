module LexicalAnalyzer (tokens) where

import Token
import Text.Regex

type TokenRegex = (String -> Token, Regex)
regexs :: [TokenRegex]
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

data TokenMatch = TokenMatch (Token, String, String, String)
instance Eq TokenMatch where
  (==) (TokenMatch (_, b, m, _)) (TokenMatch (_, b', m', _)) =
    length b >= length b' && length m >= length m'
instance Ord TokenMatch where
  (<=) (TokenMatch (_, b, m, _)) (TokenMatch (_, b', m', _)) =
    length b <= length b' && length m <= length m'

match :: String -> TokenRegex -> Maybe TokenMatch
match text (token, regex) = (matchRegexAll regex text) >>= (match')
  where match' (before, matched, after, _) =
          Just (TokenMatch (token matched, before, matched, after))

matchNext :: [Maybe TokenMatch] -> Maybe TokenMatch
matchNext = foldl maybeMax Nothing
  where
    maybeMax (Just first) (Just second) = Just (max first second)
    maybeMax first Nothing = first
    maybeMax Nothing second = second

tokens :: String -> [Token]
tokens text = []
