module Lib
  ( run,
  )
where

import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data LexicalValue = Number Int | Variable String deriving (Eq, Show)

data Token = Open | Close | Element LexicalValue deriving (Eq, Show)

lexer :: String -> LexicalValue
lexer s
  | all isNumber s = Number . read $ s
  | otherwise = Variable s

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

run :: String -> String
run = show . parse . tokenize
