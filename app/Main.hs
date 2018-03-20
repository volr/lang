module Main where

import Volr.Parser

main :: IO ()
main = case parse "Hello" of
  Left e -> putStrLn (show e)
  Right _ -> putStrLn "Success"
