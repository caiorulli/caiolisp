module Main (main) where

import Control.Monad.State

import Test.Hspec
import FrontEnd
import BackEnd
import Primitives

run :: String -> Type
run expr = fst $ foldl foldFn (Nil, initialEnv) states
  where
    foldFn :: (Type, Environment) -> StateT Environment (Either String) Type -> (Type, Environment)
    foldFn = \(_, env) st -> case runStateT st env of
      Left  _           -> (Nil, env)
      Right (t, newEnv) -> (t, newEnv)
    states = fmap eval sexprs
    sexprs = parse . tokenize $ expr

main :: IO ()
main = hspec $ do
  describe "Base functionality" $ do
    it "World makes sense" $
      run "(+ 2 2)" `shouldBe` Number 4

    it "Nested arithmetic works" $
      run "(- (+ 30 20) 8)" `shouldBe` Number 42

    it "Conditional operations work: true" $
      run "(if (= (+ 2 2) 4) 100 0)" `shouldBe` Number 100

    it "Conditional operations work: false" $
      run "(if (= (+ 2 2) 5) 100 0)" `shouldBe` Number 0

    it "Can use anonymous functions" $
      run "((fn (x) (+ x 1)) 68)" `shouldBe` Number 69

    it "Can define variables" $
      run "(def a 3) \
          \(+ a 5)" `shouldBe` Number 8

    it "Can define functions" $
      run "(defn inc (x) (+ x 1)) \
          \(inc 5)" `shouldBe` Number 6
