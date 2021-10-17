module Main where

import BackEnd
import Control.Monad.State
import FrontEnd
import Primitives
import Text.Megaparsec (errorBundlePretty, parse)

repl :: Environment -> IO ()
repl env = do
  putStr "> "
  input <- getLine
  if input == "exit"
    then return ()
    else do
      let parseResult = parse nparser "repl" input
      case parseResult of
        Left parserErrors -> do
          putStrLn $ errorBundlePretty parserErrors
          repl env
        Right sexprs -> do
          let evalResult = runStateT (mapM eval sexprs) initialEnv
          case evalResult of
            Left s -> do
              putStrLn s
              repl env
            Right (ts, newEnv) -> do
              mapM_ print ts
              repl newEnv

main :: IO ()
main = do
  putStrLn "Welcome to Caiolisp!"
  putStrLn "Have fun, and remember your most important goal... conquest!"
  repl initialEnv
