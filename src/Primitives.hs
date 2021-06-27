{-# LANGUAGE LambdaCase #-}

module Primitives
  ( initialEnv)
  where

import qualified Data.Map as M

import BackEnd

plus :: Type -> Either String Type
plus (Number a) = Right $ Fn plusA
  where
    plusA = \case
      (Number b) -> Right $ Number (a + b)
      _ -> Left "Primitive sum type error"
plus _ = Left "Primitive sum type error"

minus :: Type -> Either String Type
minus (Number a) = Right $ Fn minusA
  where
    minusA = \case
      (Number b) -> Right $ Number (a - b)
      _ -> Left "Primitive subtraction type error"
minus _ = Left "Primitive subtraction type error"

eq :: Type -> Either String Type
eq (Number a) = Right $ Fn eqA
  where
    eqA = \case
      (Number b) -> Right $ CLBool (a == b)
      _ -> Left "Primitive equals type error"
eq _ = Left "Primitive equals type error"

cons :: Type -> Either String Type
cons a = Right $ Fn (Right . Pair a)

car :: Type -> Either String Type
car (Pair a _) = Right a
car _ = Left "Primitive car type error"

cdr :: Type -> Either String Type
cdr (Pair _ b) = Right b
cdr _ = Left "Primitive cdr type error"

isPair :: Type -> Either String Type
isPair (Pair _ _) = Right $ CLBool True
isPair _ = Right $ CLBool False

isNil :: Type -> Either String Type
isNil Nil = Right $ CLBool True
isNil _ = Right $ CLBool False

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
