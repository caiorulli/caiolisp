module Main where

import Lib (run)

repl :: IO ()
repl = do
  putStr "> "
  input <- getLine
  if input == "exit"
    then return ()
    else do
      putStrLn . run $ input
      repl

main :: IO ()
main = do
  putStrLn "Welcome to Caiolisp!"
  putStrLn "Have fun, and remember your most important goal... conquest!"
  repl
