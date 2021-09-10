module Main (main) where

import Control.Monad.State

import Test.Hspec
import FrontEnd
import BackEnd
import Primitives

run :: String -> Environment -> Either String (Type, Environment)
run expr = runStateT (eval . head . parse . tokenize $ expr)

main :: IO ()
main = hspec $ do
  describe "Caiolisp" $ do
    it "Sum works" $
      run "(+ 2 2)" initialEnv `shouldBe` Right (Number 4, initialEnv)
