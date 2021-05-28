{-# LANGUAGE LambdaCase #-}

module BackEnd
  ( eval
  , Type
  , Environment
  , initialEnv
  )
where

import qualified Data.Map as M

import FrontEnd

type Variable = String

data Type
  = Number Int
  | CLBool Bool
  | Fn (Type -> Type)

instance Show Type where
  show (Number n) = show n
  show (CLBool True) = "true"
  show (CLBool False) = "false"
  show (Fn _) = "<Fn>"

type Environment = M.Map Variable Type

eval :: Environment -> Sexpr -> (Type, Environment)
eval env (Atom (IntLiteral i)) = (Number i, env)

eval env (Atom (Symbol v)) =
  case M.lookup v env of
    (Just v) -> (v, env)
    Nothing -> error "Variable not found"

eval env (Node [Atom (Symbol "def"), Atom (Symbol name), body]) =
  let (value, _) = eval env body
   in (value, M.insert name value env)

eval env (Node [Atom (Symbol "fn"), Atom (Symbol arg), body]) =
  let fn = \value -> fst $ eval (M.insert arg value env) body
   in (Fn fn, env)

eval env (Node [Atom (Symbol "if"), predicate, consequent, alternative]) =
  let (result, _) = eval env predicate
   in case result of (CLBool False) -> eval env alternative
                     _ -> eval env consequent

eval env (Node (operator : operands)) =
  let (fn, _) = eval env operator
      args = map (fst . eval env) operands
   in (apply fn args, env)

apply :: Type -> [Type] -> Type
apply = foldl next
  where
    next (Fn fn) value = fn value
    next not_a_function _ = error ("Cannot apply non-function " ++ show not_a_function)

-- Primitives

plus :: Type -> Type
plus (Number a) = Fn plusA
  where
    plusA = \case
      (Number b) -> Number (a + b)
      _ -> error "Primitive sum type error"
plus _ = error "Primitive sum type error"

minus :: Type -> Type
minus (Number a) = Fn minusA
  where
    minusA = \case
      (Number b) -> Number (a + b)
      _ -> error "Primitive subtraction type error"
minus _ = error "Primitive subtraction type error"

eq :: Type -> Type
eq (Number a) = Fn eqA
  where
    eqA = \case
      (Number b) -> CLBool (a == b)
      _ -> error "Primitive equals type error"
eq _ = error "Primitive equals type error"

initialEnv :: Environment
initialEnv =
  M.fromList
    [ ("+", Fn plus)
    , ("-", Fn minus)
    , ("=", Fn eq) 
    ]
