module Main where

import Parser.Lexer
import qualified Data.Text as T
import Data.Foldable
import Parser.Parser 

main :: IO ()
main = do 
  cs <- T.pack <$> readFile "./programs/fibo.md" 
  print . scanMany $ cs 
  case runAlex cs parse of 
    Left e -> print e
    Right xs -> traverse_ print xs
