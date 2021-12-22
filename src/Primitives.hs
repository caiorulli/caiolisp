{-# LANGUAGE LambdaCase #-}

module Primitives
  ( initialEnv)
  where

import qualified Data.Map as M

import BackEnd
    ( Environment, Type(CLFn, CLInt, CLPair, CLNil, CLBool) )

plus :: Type -> Either String Type
plus (CLInt a) = Right $ CLFn plusA
  where
    plusA = \case
      (CLInt b) -> Right $ CLInt (a + b)
      _ -> Left "Primitive sum type error"
plus _ = Left "Primitive sum type error"

minus :: Type -> Either String Type
minus (CLInt a) = Right $ CLFn minusA
  where
    minusA = \case
      (CLInt b) -> Right $ CLInt (a - b)
      _ -> Left "Primitive subtraction type error"
minus _ = Left "Primitive subtraction type error"

eq :: Type -> Either String Type
eq (CLInt a) = Right $ CLFn eqA
  where
    eqA = \case
      (CLInt b) -> Right $ CLBool (a == b)
      _ -> Left "Primitive equals type error"
eq _ = Left "Primitive equals type error"

cons :: Type -> Either String Type
cons a = Right $ CLFn (Right . CLPair a)

car :: Type -> Either String Type
car (CLPair a _) = Right a
car _ = Left "Primitive car type error"

cdr :: Type -> Either String Type
cdr (CLPair _ b) = Right b
cdr _ = Left "Primitive cdr type error"

isPair :: Type -> Either String Type
isPair (CLPair _ _) = Right $ CLBool True
isPair _ = Right $ CLBool False

isNil :: Type -> Either String Type
isNil CLNil = Right $ CLBool True
isNil _ = Right $ CLBool False

initialEnv :: Environment
initialEnv =
  M.fromList
    [ ("+", CLFn plus)
    , ("-", CLFn minus)
    , ("=", CLFn eq)
    , ("cons", CLFn cons)
    , ("car", CLFn car)
    , ("cons", CLFn cons)
    , ("isPair", CLFn isPair)
    , ("isNil", CLFn isNil)
    ]
