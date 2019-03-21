module LexicalAnalyzer (tokens) where

import Token
import Data.Maybe
import Control.Monad.State
import qualified Text.Regex as R

match :: String -> String -> Bool
match regex = isJust . (R.matchRegex (R.mkRegex regex))

isComment = match ";.*$"
isMultilineComment = match "\\|\\#(.|\n)*?\\#\\|"
isOpenParenthesis = match "("
isCloseParenthesis = match ")"
isQuote = match "'|quote"
isDefine = match "define"
isNumber = match "[0-9]"
isLiteral = match "\"((\\\")|[^\"])*\""
isSymbol = match "[^\\s()\\[\\]{}'\";]+"

classify :: String -> Maybe Token
classify token
  | isComment token = Nothing
  | isMultilineComment token = Nothing
  | isOpenParenthesis token = Just OpenParenthesis
  | isCloseParenthesis token = Just CloseParenthesis
  | isQuote token = Just Quote
  | isDefine token = Just Define
  | isNumber token = Just . Number $ token
  | isLiteral token = Just . Literal $ token
  | isSymbol token = Just . Symbol $ token
  | otherwise = Nothing

bufferToken :: String -> State (Maybe Token, String) String
bufferToken (char:blob) = do
  (token, buffer) <- get
  let buffer' = buffer ++ [char]
      token' = classify buffer'
  if isNothing token'
    then do
    put (token', "")
    return blob
    else do
    put (token', buffer')
    bufferToken blob
bufferToken _ = return ""

tokens :: String -> [Token]
tokens text = [Literal "foo"]
