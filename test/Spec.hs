module Main (main) where

import BackEnd (Type (Number), eval)
import Control.Monad.State.Lazy (evalStateT)
import FrontEnd (nparser)
import Primitives (initialEnv)
import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Megaparsec (parse, ParseErrorBundle (bundleErrors))
import Data.Void (Void)
import Data.Bifunctor (first)
import Data.List.NonEmpty (toList)

run :: String -> Either (ParseErrorBundle String Void) Type
run expr = case result of
  Left bundle -> Left bundle -- parse error
  Right (Left _) -> undefined -- eval error
  Right (Right []) -> undefined -- no result types
  Right (Right ts) -> Right $ last ts
  where
    result = (`evalStateT` initialEnv) . sequence <$> states
    states = sequence $ mapM (fmap eval) sexprs
    sexprs = parse nparser "test" expr

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
      run
        "(def a 3) \
        \(+ a 5)"
        `shouldBe` Right (Number 8)

    it "Can define functions" $
      run
        "(defn inc (x) (+ x 1)) \
        \(inc 5)"
        `shouldBe` Right (Number 6)

  describe "Errors" $ do
    it "Not closing parens should make it fail" $
      actual `shouldBe` expected
      where
        actual = first (toList . bundleErrors) (run "(+ 0 1")
        expected = Left []
