module Main where

import Control.Monad.State

import FrontEnd
import BackEnd
import Primitives 
  
repl :: Environment -> IO ()
repl env = do
  putStr "> "
  input <- getLine
  if input == "exit"
    then return ()
    else do
      let sexpr = head . parse . tokenize $ input
          (result, newEnv) = runStateT (eval sexpr) env
      print result
      repl newEnv

main :: IO ()
main = do
  putStrLn "Welcome to Caiolisp!"
  putStrLn "Have fun, and remember your most important goal... conquest!"
  repl initialEnv
