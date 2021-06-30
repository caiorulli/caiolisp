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
  | Fn (Type -> Either String Type)

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

eval :: Sexpr -> StateT Environment (Either String) Type
eval sexpr = case sexpr of
  (Atom (IntLiteral i)) -> lift . return  $ Number i

  (Atom (Symbol v)) -> do
    env <- get
    case M.lookup v env of
      (Just v) -> return v
      Nothing -> lift . Left $ "Variable not found: " ++ show v

  (Node [Atom (Symbol "def"), Atom (Symbol name), body]) -> do
    value <- eval body
    modify (M.insert name value)
    return value

  (Node [Atom (Symbol "fn"), Node args, body]) -> do
    env <- get
    lift $ makeFn env args body

  (Node [Atom (Symbol "defn"), Atom (Symbol name), Node args, body]) -> do
    env <- get
    valueFn <- lift $ makeFn env args body
    modify (M.insert name valueFn)
    return valueFn

  (Node [Atom (Symbol "if"), predicate, consequent, alternative]) -> do
    result <- eval predicate
    if result == CLBool False
      then eval alternative
      else eval consequent

  (Node (operator : operands)) -> do
    fn <- eval operator
    args <- mapM eval operands
    lift $ apply fn args

  _ -> lift $ Left "Unrecognized construction"

makeFn :: Environment -> [Sexpr] -> Sexpr -> Either String Type
makeFn env [] body = evalStateT (eval body) env
makeFn env (Atom (Symbol x):args) body = return $
  Fn (\value -> makeFn (M.insert x value env) args body)
makeFn _ _ _ = Left "Wrong types"

apply :: Type -> [Type] -> Either String Type
apply a = foldl next (Right a)
  where
    next (Left errorStr) _ = Left errorStr
    next (Right (Fn fn)) value = fn value
    next not_a_function _ = Left ("Cannot apply non-function " ++ show not_a_function)
