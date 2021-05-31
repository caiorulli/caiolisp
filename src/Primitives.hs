{-# LANGUAGE LambdaCase #-}

module Primitives
  ( initialEnv)
  where

import qualified Data.Map as M

import BackEnd

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
      (Number b) -> Number (a - b)
      _ -> error "Primitive subtraction type error"
minus _ = error "Primitive subtraction type error"

eq :: Type -> Type
eq (Number a) = Fn eqA
  where
    eqA = \case
      (Number b) -> CLBool (a == b)
      _ -> error "Primitive equals type error"
eq _ = error "Primitive equals type error"

cons :: Type -> Type
cons a = Fn (Pair a)

car :: Type -> Type
car (Pair a _) = a

cdr :: Type -> Type
cdr (Pair _ b) = b

isPair :: Type -> Type
isPair (Pair _ _) = CLBool True
isPair _ = CLBool False

isNil :: Type -> Type
isNil Nil = CLBool True
isNil _ = CLBool False

initialEnv :: Environment
initialEnv =
  M.fromList
    [ ("+", Fn plus)
    , ("-", Fn minus)
    , ("=", Fn eq) 
    , ("cons", Fn cons)
    , ("car", Fn car)
    , ("cons", Fn cons)
    , ("isPair", Fn isPair)
    , ("isNil", Fn isNil)
    ]
