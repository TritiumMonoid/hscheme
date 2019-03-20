module LexicalAnalyzer (tokens) where

import Token
import Data.Maybe
import Contol.Monad.State
import qualified Text.Regex as R

match :: String -> String -> Bool
match regex = isJust . (R.matchRegex (R.mkRegex regex))

isNumber = match "[0-9]+"
isSymbol = match "[A-Za-z0-9]+"
isOpenParenthesis = match "("
isCloseParenthesis = match ")"
isWhitespace = match "\\s+"

classify :: String -> Maybe Token
classify token
  | isOpenParenthesis token = Just OpenParenthesis
  | isCloseParenthesis token = Just CloseParenthesis
  | isNumber token = Just . Number $ read token
  | isSymbol token = Just . Symbol $ token
  | isWhitespace token = Nothing
  | otherwise = Nothing

bufferToken :: String -> State (Maybe Token, String) String
bufferToken (char:blob) = do
  (token, buffer) <- get
  let buffer' = buffer ++ char
      token' = classify buffer'
  if isNothing token'
    put (token', "")
    then return blob
    else do
    put (token', buffer')
    bufferToken blob
bufferToken _ = return ""

bufferTokens :: String -> State [Token] String
bufferTokens text = do
  tokens <- get
  text' <- bufferToken text
  (token, _) <- get
  put $ tokens ++ maybeToList token

tokens :: String -> [Token]
tokens text = [Literal "foo"]
