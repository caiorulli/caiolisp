module FrontEnd
  ( LexicalValue (..),
    Sexpr (..),
    parser,
    ParserErrors,
  )
where

import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import TypeSystem

data LexicalValue
  = Literal Type
  | Symbol String
  deriving (Eq, Show)

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

nil :: Parser ()
nil = void $ symbol "nil"

elementsList :: Parser [Sexpr]
elementsList = openParens *> someTill element closeParens

element :: Parser Sexpr
element =
  choice
    [ Atom . Literal . CLInt <$> integer,
      Atom (Literal CLNil) <$ nil,
      Atom . Symbol <$> otherSymbol,
      Node <$> elementsList
    ]

parser :: Parser [Sexpr]
parser = manyTill element eof
