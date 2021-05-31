module BackEnd
  ( eval
  , Type (..)
  , Environment
  )
where

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

instance Show Type where
  show (Number n) = show n
  show (CLBool True) = "true"
  show (CLBool False) = "false"
  show (CLChar c) = show c
  show (Pair a b) = "(" ++ show a ++ " . " ++ show b ++ ")" 
  show Nil = "nil"
  show (Fn _) = "<Fn>"

type Environment = M.Map Variable Type

eval :: Environment -> Sexpr -> (Type, Environment)
eval env (Atom (IntLiteral i)) = (Number i, env)

eval env (Atom (Symbol v)) =
  case M.lookup v env of
    (Just v) -> (v, env)
    Nothing -> error $ "Variable not found: " ++ show v

eval env (Node [Atom (Symbol "def"), Atom (Symbol name), body]) =
  let (value, _) = eval env body
   in (value, M.insert name value env)

eval env (Node [Atom (Symbol "fn"), Node args, body]) =
  let fn = makeFn env args body
   in (fn, env)

eval env (Node [Atom (Symbol "defn"), Atom (Symbol name), Node args, body]) =
  let valueFn = makeFn env args body
   in (valueFn, M.insert name valueFn env)

eval env (Node [Atom (Symbol "if"), predicate, consequent, alternative]) =
  let (result, _) = eval env predicate
   in case result of (CLBool False) -> eval env alternative
                     _ -> eval env consequent

eval env (Node (operator : operands)) =
  let (fn, _) = eval env operator
      args = map (fst . eval env) operands
   in (apply fn args, env)

makeFn :: Environment -> [Sexpr] -> Sexpr -> Type
makeFn env [] body = fst $ eval env body
makeFn env (Atom (Symbol x):args) body =
  Fn (\value -> makeFn (M.insert x value env) args body)
  
apply :: Type -> [Type] -> Type
apply = foldl next
  where
    next (Fn fn) value = fn value
    next not_a_function _ = error ("Cannot apply non-function " ++ show not_a_function)
