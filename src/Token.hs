module Token where

data Token =
  OpenParenthesis
  | CloseParenthesis
  | Symbol String
  | Number String
  | Literal String
  | Whitespace
  | Comment
  deriving (Eq, Show)
