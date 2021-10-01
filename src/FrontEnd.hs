module FrontEnd
  ( LexicalValue (..)
  , Token (..)
  , Sexpr (..)
  , oparse
  , nparser
  , tokenize)
  where

import Data.Char ( isNumber )
import Data.Functor (($>))
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, manyTill, choice, eof)
import Text.Megaparsec.Char (space1, char)
import Control.Applicative (Alternative(empty, (<|>), many))
import Control.Monad (void)

data LexicalValue = IntLiteral Integer | Symbol String deriving (Eq, Show)

data Sexpr = Atom LexicalValue | Node [Sexpr] deriving (Show)

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

otherSymbol :: Parser String
otherSymbol = manyTill L.charLiteral $
  choice [space1, eof, void openParens, void closeParens]

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer = lexeme L.decimal

openParens :: Parser Char
openParens = lexeme $ char '('

closeParens :: Parser Char
closeParens = lexeme $ char ')'

element :: Parser Sexpr
element = choice
  [ Atom . IntLiteral <$> integer
  -- , Atom . Symbol <$> otherSymbol
  , Node <$> (openParens *> manyTill element closeParens)
  ]

nparser :: Parser [Sexpr]
nparser = many element

-- Old parser

data Token = Open | Close | Element LexicalValue deriving (Eq, Show)

type Depth = Int

lexer :: String -> LexicalValue
lexer s
  | all isNumber s = IntLiteral . read $ s
  | otherwise = Symbol s

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

oparse :: [Token] -> [Sexpr]
oparse [] = []
oparse ((Element lexVal) : ts) = Atom lexVal : oparse ts
oparse (Open : ts) =
  let (sexpr, rest) = breakOnMatchingClose 0 ts
   in Node (oparse sexpr) : oparse rest
oparse (Close : ts) = oparse ts
