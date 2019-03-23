module LexicalAnalyzer (tokens) where

import Token
import Data.Maybe
import Data.List
import Control.Monad.State
import qualified Text.Regex as R

match :: String -> String -> Bool
match regex = isJust . (R.matchRegex (R.mkRegex regex))

isComment = match ";.*$"
isMultilineComment = match "\\#\\|(.|\n)*?\\|\\#"
isOpenParenthesis = match "("
isCloseParenthesis = match ")"
isQuote = match "'|quote"
isDefine = match "define"
isNumber = match "[0-9]+"
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

bufferToken :: String -> State (String, Maybe Token) String
bufferToken (char:blob) = do
  (buffer, _) <- get
  let buffer' = buffer ++ [char]
      token'  = classify buffer'
  if isNothing token'
    then do
    put ("", Nothing)
    return blob
    else do
    put (buffer', token')
    bufferToken blob
bufferToken = return

bufferTokens :: String -> State [Token] String
bufferTokens blob = do
  --tokens <- get
  let (token, _) = runState (bufferToken blob) ("", Nothing)
  put ([] ++ [token])
  return "ok"

tokens :: String -> [Token]
tokens blob = snd $ (runState (bufferTokens blob) [])
