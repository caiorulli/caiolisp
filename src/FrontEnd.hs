module FrontEnd
  ( LexicalValue (..),
    Sexpr (..),
    parser,
    ParserErrors,
  )
where

import Control.Monad (void)
import Data.Char (isNumber)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec
  ( ParseErrorBundle,
    Parsec,
    between,
    choice,
    empty,
    eof,
    many,
    manyTill,
    noneOf,
    some,
    someTill,
    try,
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, symbolChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)

data LexicalValue = IntLiteral Integer | Symbol String deriving (Eq, Show)

data Sexpr = Atom LexicalValue | Node [Sexpr] deriving (Show)

type Parser = Parsec Void String

type ParserErrors = ParseErrorBundle String Void

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
variableChar = letterChar <|> symbolChar <|> char '-'

otherSymbol :: Parser String
otherSymbol = lexeme $ some variableChar

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

elementsList :: Parser [Sexpr]
elementsList = openParens *> someTill element closeParens

element :: Parser Sexpr
element =
  choice
    [ Atom . IntLiteral <$> integer,
      Atom . Symbol <$> otherSymbol,
      Node <$> elementsList
    ]

parser :: Parser [Sexpr]
parser = manyTill element eof
