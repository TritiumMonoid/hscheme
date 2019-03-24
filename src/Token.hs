module Token where

data Token =
  OpenParenthesis
  | CloseParenthesis
  | Define
  | Quote
  | Symbol String
  | Number String
  | Literal String
  | Whitespace
  | Comment
  deriving (Eq, Show)
