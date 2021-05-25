module Lib
    ( run
    ) where

import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data LexicalValue = Number Int | Variable String deriving (Eq, Show)
data Token = Open | Close | Element LexicalValue deriving (Eq, Show)

lexer :: String -> LexicalValue
lexer s
  | all isNumber s = Number . read $ s
  | otherwise      = Variable s

openOrClose :: Char -> Bool
openOrClose '(' = True
openOrClose ')' = True
openOrClose _   = False

tokenizeWord :: String -> [Token]
tokenizeWord [] = []
tokenizeWord s@(c:cs)
  | c == '('  = Open : tokenizeWord cs
  | c == ')'  = Close : tokenizeWord cs
  | otherwise = let (element, rest) = break openOrClose s
                in  Element (lexer element) : tokenizeWord rest

tokenize :: String -> [Token]
tokenize input =
  let inputWords = words input
  in concatMap tokenizeWord inputWords

data Sexpr = Atom LexicalValue | Node [Sexpr] deriving (Show)

parse :: [Token] -> [Sexpr]
parse [] = []
parse ((Element lexVal):ts) = Atom lexVal : parse ts
parse (Open:ts) = let (sexpr, rest) = break (== Close) ts
                  in Node (parse sexpr) : parse rest
parse (Close:ts) = parse ts
  
run :: String -> String
run = show . parse . tokenize
