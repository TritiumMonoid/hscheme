module LexicalAnalyzer (tokens) where

import Token
import Data.Maybe
import Data.List
import qualified Text.Regex as R

comment = (R.mkRegex ";.*$", const Comment)
multilineComment = (R.mkRegex "\\|\\#(.|\n)*?\\#\\|", const Comment)
openParenthesis = (R.mkRegex "(", const OpenParenthesis)
closeParenthesis = (R.mkRegex ")", const CloseParenthesis)
quote = (R.mkRegex "'|quote", const Quote)
define = (R.mkRegex "define", const Define)
whitespace = (R.mkRegex "\\s+", const Whitespace)
number = (R.mkRegex "[0-9]+", \text -> Number text)
literal = (R.mkRegex "\"((\\\")|[^\"])*\"", \text -> Literal text)
symbol = (R.mkRegex "[^\\s()\\[\\]{}'\";]+", \text -> Symbol text)

nextMatch :: (R.Regex, String -> Token) -> String -> Maybe (String, Token, String)
nextMatch (regex, token) text =
  let match = R.matchRegexAll regex text
      match' (Just (before, matched, after, _)) = Just (before, token matched, after)
      match' _ = Nothing
  in Nothing

tokens :: String -> [Token]
tokens text = []
