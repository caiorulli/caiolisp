module Main (main) where

import Control.Monad.State

import Test.Hspec
import FrontEnd
import BackEnd
import Primitives

foldStates :: Environment -> [StateT Environment (Either String) Type]
  -> Either String Type
foldStates _ [] = Right Nil
foldStates env [s] = evalStateT s env
foldStates env (s:ss) = case result of
  Right resultEnv -> foldStates resultEnv ss
  Left resultErr -> Left resultErr
  where result = execStateT s env

run :: String -> Either String Type
run expr = foldStates initialEnv $ fmap eval sexprs
  where sexprs = parse . tokenize $ expr

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

    it "Can define variables" $
      run "(def a 3) \
          \(+ a 5)" `shouldBe` Right (Number 8)

    it "Can define functions" $
      run "(defn inc (x) (+ x 1)) \
          \(inc 5)" `shouldBe` Right (Number 6)
