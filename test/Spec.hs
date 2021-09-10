module Main (main) where

import Control.Monad.State

import Test.Hspec
import FrontEnd
import BackEnd
import Primitives

run :: String -> Either String Type
run expr = fmap fst result
  where result = runStateT (eval . head . parse . tokenize $ expr) initialEnv

main :: IO ()
main = hspec $ do
  describe "Base functionality" $ do
    it "World makes sense" $
      run "(+ 2 2)" `shouldBe` Right (Number 4)

    it "Nested arithmetic works" $
      run "(- (+ 30 20) 8)" `shouldBe` Right (Number 42)

    it "Conditional operations work: true" $
      run "(if (= (+ 2 2) 4) 100 0)" `shouldBe` Right (Number 100)

    it "Conditional operations work: false" $
      run "(if (= (+ 2 2) 5) 100 0)" `shouldBe` Right (Number 0)

    it "Can use anonymous functions" $
      run "((fn (x) (+ x 1)) 68)" `shouldBe` Right (Number 69)
