module Main where

import Parser.Lexer
import qualified Data.Text as T
import Parser.LexerDefinitions
import Data.Foldable
main :: IO ()
main = readFile "./programs/string.md" >>=  print . scanMany . T.pack

