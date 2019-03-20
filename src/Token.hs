module Token where

data Token =
  OpenParenthesis
  | CloseParenthesis
  | Symbol String
  | Number Int
  | Literal String
  deriving (Eq, Show)
