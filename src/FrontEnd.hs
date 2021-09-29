module FrontEnd
  ( LexicalValue (..)
  , Token (..)
  , Sexpr (..)
  , parse
  , tokenize)
  where

import Data.Char ( isNumber )
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Text.Megaparsec (Parsec, between)
import Text.Megaparsec.Char (space1, char)
import Control.Applicative (Alternative(empty))
import Text.ParserCombinators.ReadP (manyTill)

data LexicalValue = IntLiteral Int | Symbol String deriving (Eq, Show)

data Token = Open | Close | Element LexicalValue deriving (Eq, Show)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment ";;")
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

lexer :: String -> LexicalValue
lexer s
  | all isNumber s = IntLiteral . read $ s
  | otherwise = Symbol s

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

tokenizeWord :: String -> [Token]
tokenizeWord [] = []
tokenizeWord s@(c : cs)
  | c == '(' = Open : tokenizeWord cs
  | c == ')' = Close : tokenizeWord cs
  | otherwise =
    let (element, rest) = break (\c -> c == '(' || c == ')') s
     in Element (lexer element) : tokenizeWord rest

tokenize :: String -> [Token]
tokenize input =
  let inputWords = words input
   in concatMap tokenizeWord inputWords

data Sexpr = Atom LexicalValue | Node [Sexpr] deriving (Show)

type Depth = Int

breakOnMatchingClose :: Depth -> [Token] -> ([Token], [Token])
breakOnMatchingClose 0 (Close : ts) = ([], ts)
breakOnMatchingClose depth (Open : ts) =
  let (nextA, nextB) = breakOnMatchingClose (depth + 1) ts
   in (Open : nextA, nextB)
breakOnMatchingClose depth (Close : ts) =
  let (nextA, nextB) = breakOnMatchingClose (depth - 1) ts
   in (Close : nextA, nextB)
breakOnMatchingClose depth (t : ts) =
  let (nextA, nextB) = breakOnMatchingClose depth ts
   in (t : nextA, nextB)

parse :: [Token] -> [Sexpr]
parse [] = []
parse ((Element lexVal) : ts) = Atom lexVal : parse ts
parse (Open : ts) =
  let (sexpr, rest) = breakOnMatchingClose 0 ts
   in Node (parse sexpr) : parse rest
parse (Close : ts) = parse ts
