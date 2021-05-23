module Lib
    ( eval
    ) where

import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Expression = Number Int | Symbol String | Expression [Expression]
  deriving (Show)

analyze :: String -> Expression
analyze ('(':substr) = Expression . map analyze . words . takeWhile (/=')') $
  substr
analyze s
  | all isNumber s = Number . read $ s
  | otherwise      = Symbol s
          
eval :: String -> String
eval = show . analyze

evalExpression :: Expression -> Expression
evalExpression (Expression (operator:operands)) =
  apply (evalExpression operator) (map evalExpression operands)
evalExpression expr = expr

apply :: Expression -> [Expression] -> Expression
apply _ _ = Number 1
