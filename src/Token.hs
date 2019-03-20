module Token where

data Token =
  OpenParenthesis
  | CloseParenthesis
  | Symbol String
  | Number Int
  | Literal String
  | Whitespace
  deriving (Eq, Show)
