{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Parser.Lexer
import qualified Data.Text as T
import Data.Foldable
import Parser.Parser 
import Parser.ScopeCheck
import System.Environment (getArgs)
import System.Path.Glob (glob)
import Control.Monad 

-- main :: IO ()
-- main = getArgs >>= \as -> forM_ as $ \a -> (putStrLn $ a <> "\n" ) >> glob a >>= \fs -> forM fs $ \f -> readFile f >>=  \(T.pack -> cs) -> do 
--   print . scanMany $ cs 
--   case runAlex cs parse of 
--     Left e -> print e
--     Right xs -> traverse_ print xs >> runScopeIO xs >>= \case 
--       [] -> putStrLn "No scoping errors!"
--       es  -> putStrLn "------\n\nScoping errors:\n" >> traverse_ print es
--   putStrLn "\n\n"

import Nested 

main :: IO ()
main = runScopeIO defs >>= \case 
  [] -> putStrLn "No scoping errors!"
  es  -> putStrLn "------\n\nScoping errors:\n" >> traverse_ print es

