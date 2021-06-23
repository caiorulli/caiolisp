module BackEnd
  ( eval
  , Type (..)
  , Environment
  )
where

import Control.Monad.State
import qualified Data.Map as M

import FrontEnd

type Variable = String

data Type
  = Number Int
  | CLBool Bool
  | CLChar Char
  | Pair Type Type
  | Nil
  | Fn (Type -> Type)

instance Eq Type where
  (CLBool a) == (CLBool b) = a == b
  _ == _ = False

instance Show Type where
  show (Number n) = show n
  show (CLBool True) = "true"
  show (CLBool False) = "false"
  show (CLChar c) = show c
  show (Pair a b) = "(" ++ show a ++ " . " ++ show b ++ ")"
  show Nil = "nil"
  show (Fn _) = "<Fn>"

type Environment = M.Map Variable Type

eval :: Sexpr -> State Environment Type
eval (Atom (IntLiteral i)) = return (Number i)

eval (Atom (Symbol v)) = do
  env <- get
  case M.lookup v env of
    (Just v) -> return v
    Nothing -> error $ "Variable not found: " ++ show v

eval (Node [Atom (Symbol "def"), Atom (Symbol name), body]) = do
  value <- eval body
  modify (M.insert name value)
  return value

eval (Node [Atom (Symbol "fn"), Node args, body]) = do
  env <- get
  return (makeFn env args body)

eval (Node [Atom (Symbol "defn"), Atom (Symbol name), Node args, body]) = do
  env <- get
  let valueFn = makeFn env args body
  modify (M.insert name valueFn)
  return valueFn

eval (Node [Atom (Symbol "if"), predicate, consequent, alternative]) = do
  result <- eval predicate
  if result == CLBool False
    then eval alternative
    else eval consequent

eval (Node (operator : operands)) = do
  fn <- eval operator
  args <- mapM eval operands
  return (apply fn args)

makeFn :: Environment -> [Sexpr] -> Sexpr -> Type
makeFn env [] body = evalState (eval body) env
makeFn env (Atom (Symbol x):args) body =
  Fn (\value -> makeFn (M.insert x value env) args body)
makeFn _ _ _ = error "Wrong types"

apply :: Type -> [Type] -> Type
apply = foldl next
  where
    next (Fn fn) value = fn value
    next not_a_function _ = error ("Cannot apply non-function " ++ show not_a_function)
