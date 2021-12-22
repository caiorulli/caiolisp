module TypeSystem
  ( Type (..),
  )
where

data Type
  = CLInt Integer
  | CLBool Bool
  | CLChar Char
  | CLPair Type Type
  | CLNil
  | CLFn (Type -> Either String Type)

instance Eq Type where
  (CLBool a) == (CLBool b) = a == b
  (CLInt a) == (CLInt b) = a == b
  CLNil == CLNil = True
  _ == _ = False

instance Show Type where
  show (CLInt n) = show n
  show (CLBool True) = "true"
  show (CLBool False) = "false"
  show (CLChar c) = show c
  show (CLPair a b) = "(" ++ show a ++ " . " ++ show b ++ ")"
  show CLNil = "nil"
  show (CLFn _) = "<Fn>"
