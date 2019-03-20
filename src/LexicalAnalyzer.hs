module LexicalAnalyzer (tokens) where

--import Token
--import Data.Maybe
import qualified Data.Text as T
--import qualified Text.Regex as R

--isNumber :: String -> Bool
--isNumber = isJust . R.matchRegex (R.mkRegex "^[0-9]+$")

--isSymbol :: String -> Bool
--isSymbol = isJust . R.matchRegex (R.mkRegex "^.+$")

tokens :: String -> [String]
tokens text =
  map T.unpack
  $ filter (\token -> not (token == (T.pack " ") || token == (T.pack "")))
  $ T.splitOn (T.pack " ")
  $ T.replace (T.pack ")") (T.pack " ) ")
  $ T.replace (T.pack "(") (T.pack " ( ")
  $ T.pack text
