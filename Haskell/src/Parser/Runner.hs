module Parser.Runner where

import Parser.Lexer
import Parser.LexerDefinitions
import Parser.Parser
import Data.Text (Text)
import qualified Data.Text as T

generateNested :: String 
generateNested = "int f(){\n  int x := 5;\n" <> go 1000 <> "\n}" 
  where 
    go 0 = "int coso0 := 0; int coso1 := 1; int coso2 := 2; int coso3 := 3;"
    go n = "for(y_" <> show n <> " : x){\n" <> go (n-1) <> "\n};"

generateNestedProgram :: IO ()
generateNestedProgram = writeFile "./programs/nested" generateNested

generateNestedHs :: IO ()
generateNestedHs = case runAlex (T.pack generateNested) parse of 
  Left e -> print e
  Right xs -> writeFile "./src/Nested.hs" 
    $  "{-# LANGUAGE OverloadedStrings #-}\n"
    <> "module Nested where\n"
    <> "import Parser.Lexer\n"
    <> "import Parser.LexerDefinitions\n"
    <> "import Parser.Parser\n"
    <> "import Data.Text (Text)\n"
    <> "import qualified Data.Text as T\n\n"
    <> "defs :: [Definition AlexPosn]\n"
    <> "defs = " <> show xs <> "\n"
    
