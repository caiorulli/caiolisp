module Lib
  ( run,
  )
where

import Data.Char
import qualified Data.Map as M

-- Front-end
  
data LexicalValue = IntLiteral Int | Symbol String deriving (Eq, Show)

data Token = Open | Close | Element LexicalValue deriving (Eq, Show)

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

-- Backend

type Variable = String

data Type
  = Number Int
  | Fn0 Type
  | Fn1 (Type -> Type)
  | Fn2 (Type -> Type -> Type)
  | Fn3 (Type -> Type -> Type -> Type)

instance Show Type where
  show (Number n) = show n
  show (Fn0 _) = "<Fn>"
  show (Fn1 _) = "<Fn>"
  show (Fn2 _) = "<Fn>"
  show (Fn3 _) = "<Fn>"

type Environment = M.Map Variable Type

eval :: Environment -> Sexpr -> Type
eval env (Node (operator : operands)) =
  apply (eval env operator) (map (eval env) operands)
eval env (Atom (IntLiteral i)) = Number i
eval env (Atom (Symbol v)) =
  case M.lookup v env of (Just v) -> v
                         Nothing  -> error "Variable not found"

apply :: Type -> [Type] -> Type
apply (Fn0 operator) _ = operator
apply (Fn1 operator) (a:_) = operator a
apply (Fn2 operator) (a:b:_) = operator a b
apply (Fn3 operator) (a:b:c:_) = operator a b c
apply a b = error $ "Tried to apply non-function type: " ++ show a

-- Primitives

plus :: Type -> Type -> Type
plus (Number a) (Number b) = Number (a + b)
plus _ _ = error "Cannot sum types other than Number"

minus :: Type -> Type -> Type
minus (Number a) (Number b) = Number (a - b)
minus _ _ = error "Cannot subtract types other than Number"

initialEnv :: Environment
initialEnv = M.fromList
  [("+",   Fn2 plus)
  ,("-",   Fn2 minus)
  ]
  
run :: String -> String
run = show . map (eval initialEnv) . parse . tokenize
