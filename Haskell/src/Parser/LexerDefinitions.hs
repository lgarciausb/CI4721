{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GADTs               #-}
module Parser.LexerDefinitions where 

import Control.Lens.TH 
import Control.Monad.State
import Data.Text (Text)
import Data.Text qualified as T
import Control.Lens
import Control.Monad.Except

data Token' a 
  = LChar Text a  
  | LString Text a 
  | LNumber Text a
  | LAtom Text a
  | LOBckt a
  | LCBckt a
  | LOBrc a
  | LCBrc a
  | LOParen a 
  | LCParen a
  | LComma a
  | LColon a
  | LFatArrow a 
  | LAssign a
  | LMatch a
  | LWith a
  | LType a
  | LSemiColon a
  | LFor a
  | LWhile a 
  | LContinue a
  | LBreak a
  | LNew a
  | LBy a
  | LReference a
  | LOp Text  a
  | LVBar a
  | LIdentifier Text a
  | LComment Text a
  | LDot a
  | LEOF 
  deriving (Eq, Show)


data AAST a where 
  

data EAST where 

-- instance Show Token where 
--   show (LChar t)       = "'" <> T.unpack t <> "'"
--   show (LString t)     = "\"" <> T.unpack t <> "\""
--   show (LNumber t)     = T.unpack t
--   show (LAtom t)       = "#" <> T.unpack t
--   show LOBckt          = "["
--   show LCBckt          = "]"
--   show LOBrc           = "{\n"
--   show LCBrc           = "\n}"
--   show LOParen         = "("
--   show LCParen         = ")"
--   show LComma          = ", "
--   show LColon          = " : "
--   show LFatArrow       = " => "
--   show LAssign         = " := "
--   show LMatch          = "match"
--   show LWith           = "with"
--   show LType           = "type"
--   show LSemiColon      = ";\n"
--   show LFor            = "for"
--   show LWhile          = "while"
--   show LContinue       = "continue"
--   show LBreak          = "bre{-# LANGUAGE PatternSynonyms #-}ak"
--   show LNew            = "new"
--   show LBy             = "by"
--   show LReference      = "reference"
--   show (LOp t)         = T.unpack t
--   show LVBar           = "|"
--   show (LIdentifier t) = T.unpack t
--   show (LComment t)    = "/*\n" <> T.unpack t <> "\n*/"
--   show (LEOF)          = "\\EOF"

data AlexUserState = AUST 
  { _austCommentDepth :: !Int
  , _austTextBuff     :: Text 
  , _austCommentBuff  :: Text
  }

$(makeLenses ''AlexUserState)

alexInitUserState :: AlexUserState 
alexInitUserState = AUST 
  { _austCommentDepth = 0
  , _austTextBuff     = mempty 
  , _austCommentBuff  = mempty
  }

type P a = StateT AlexUserState (ExceptT Text Maybe) a

