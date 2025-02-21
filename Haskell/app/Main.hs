module Main where

import Lexer
import qualified Data.Text as T
import LexerDefinitions
main :: IO ()
main = readFile "./programs/fibo.md" >>= print . flip runAlex alexMonadScan . T.pack

