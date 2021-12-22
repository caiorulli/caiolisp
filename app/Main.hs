module Main where

import Caiolisp.BackEnd
import Caiolisp.FrontEnd
import Caiolisp.Primitives
import Control.Monad.State
import System.Console.Haskeline
import Text.Megaparsec

type Repl a = InputT IO a

process :: String -> Environment -> IO Environment
process input env =
  let parseResult = parse parser "repl" input
   in case parseResult of
        Left parserErrors -> do
          putStrLn $ errorBundlePretty parserErrors
          return env
        Right sexprs ->
          let evalResult = runStateT (mapM eval sexprs) env
           in case evalResult of
                Left s -> do
                  putStrLn s
                  return env
                Right (ts, newEnv) -> do
                  mapM_ print ts
                  return newEnv

repl :: Environment -> Repl ()
repl env = do
  input <- getInputLine "> "
  case input of
    Nothing -> repl env
    Just "exit" -> outputStrLn "See ya!"
    Just expr -> liftIO (process expr env) >>= repl

main :: IO ()
main = do
  putStrLn "Welcome to Caiolisp!"
  putStrLn "Have fun, and remember your most important goal... conquest!"
  runInputT defaultSettings $ repl initialEnv
