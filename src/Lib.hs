module Lib
    ( eval
    ) where

import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data AtomicExpr = Number Int | Symbol String

instance Show AtomicExpr where
  show (Number n) = show n
  show (Symbol s) = '\'':s

data Expression = Atomic AtomicExpr | Expression [Expression]
  deriving (Show)

analyze :: String -> Expression
analyze ('(':substr) = Expression . map analyze . words . takeWhile (/=')') $
  substr
analyze s
  | all isNumber s = Atomic . Number . read $ s
  | otherwise      = Atomic . Symbol $ s
          
eval :: String -> String
eval = show . evalExpression . analyze

evalExpression :: Expression -> AtomicExpr
evalExpression (Expression (operator:operands)) =
  apply (evalExpression operator) (map evalExpression operands)
evalExpression (Atomic expr) = expr

atomicExprToInt :: AtomicExpr -> Int
atomicExprToInt (Number i) = i
atomicExprToInt (Symbol _) = 0

apply :: AtomicExpr -> [AtomicExpr] -> AtomicExpr
apply (Symbol "+") exprs = Number . sum . map atomicExprToInt $ exprs
apply _ _ = Number 2
