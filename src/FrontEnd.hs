module FrontEnd
  ( LexicalValue (..),
    Sexpr (..),
    nparser,
  )
where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad (void)
import Data.Char (isNumber)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, between, choice, eof, manyTill, noneOf, try)
import Text.Megaparsec.Char (alphaNumChar, char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

data LexicalValue = IntLiteral Integer | Symbol String deriving (Eq, Show)

data Sexpr = Atom LexicalValue | Node [Sexpr] deriving (Show)

type Parser = Parsec Void String

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment ";;")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

variableChar :: Parser Char
variableChar = noneOf ['(', ')', ' ']

otherSymbol :: Parser String
otherSymbol = lexeme $ many variableChar

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer = lexeme L.decimal

openParens :: Parser ()
openParens = void $ symbol "("

closeParens :: Parser ()
closeParens = void $ symbol ")"

element :: Parser Sexpr
element =
  choice
    [ Atom . IntLiteral <$> integer,
      Node <$> (openParens *> manyTill element (closeParens <|> eof)),
      Atom . Symbol <$> otherSymbol
    ]

nparser :: Parser [Sexpr]
nparser = manyTill element eof
