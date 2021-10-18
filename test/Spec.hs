module Main (main) where

import BackEnd (Type (Number), eval)
import Control.Monad.State.Lazy (evalStateT)
import FrontEnd (ParserErrors, parser)
import Primitives (initialEnv)
import Test.Hspec (Expectation, describe, expectationFailure, hspec, it, shouldBe)
import Text.Megaparsec (parse)

run :: String -> Either ParserErrors Type
run expr = case result of
  Left bundle -> Left bundle
  Right (Left _) -> undefined -- eval error
  Right (Right []) -> undefined -- no result types
  Right (Right ts) -> Right $ last ts
  where
    result = (`evalStateT` initialEnv) . sequence <$> states
    states = sequence $ mapM (fmap eval) sexprs
    sexprs = parse parser "test" expr

shouldEval :: Either ParserErrors Type -> Type -> Expectation
shouldEval actual expected = case actual of
  Left _ -> expectationFailure "Did not parse"
  Right t -> t `shouldBe` expected

shouldFailParse :: Either ParserErrors Type -> Bool
shouldFailParse (Left _) = True
shouldFailParse _ = False

main :: IO ()
main = hspec $ do
  describe "Base functionality" $ do
    it "World makes sense" $
      run "(+ 2 2)" `shouldEval` Number 4

    it "Nested arithmetic works" $
      run "(- (+ 30 20) 8)" `shouldEval` Number 42

    it "Conditional operations work: true" $
      run "(if (= (+ 2 2) 4) 100 0)" `shouldEval` Number 100

    it "Conditional operations work: false" $
      run "(if (= (+ 2 2) 5) 100 0)" `shouldEval` Number 0

    it "Can use anonymous functions" $
      run "((fn (x) (+ x 1)) 68)" `shouldEval` Number 69

    it "Can define variables" $
      run
        "(def a 3) \
        \(+ a 5)"
        `shouldEval` Number 8

    it "Can define functions" $
      run
        "(defn inc (x) (+ x 1)) \
        \(inc 5)"
        `shouldEval` Number 6

  describe "Errors" $ do
    it "Not closing parens should make it fail" $
      shouldFailParse $ run "(+ 0 1"

    it "Not having any elements inside parens should make it fail" $
      shouldFailParse $ run "()"
