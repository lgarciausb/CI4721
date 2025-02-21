{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
module LexerDefinitions where 

import Control.Lens.TH 
import Control.Monad.State
import Data.Text (Text)
import Data.Text qualified as T
import Control.Lens
import Language.Haskell.TH.Syntax (Name,mkName)
import Language.Haskell.TH.Lib.Internal (DecsQ)
import Control.Monad.Except

myMakeLenses :: Name -> DecsQ
myMakeLenses = makeLensesWith $ lensRules
  & lensField .~ (\_ _ name -> [TopName $ mkName $ show name <> "'"])

data Token 
  = LChar Text  
  | LString Text 
  | LNumber Text 
  | LAtom Text 
  | LOBckt 
  | LCBckt 
  | LOBrc 
  | LCBrc 
  | LOParen 
  | LCParen 
  | LComma 
  | LColon 
  | LFatArrow 
  | LAssign 
  | LMatch 
  | LWith 
  | LType 
  | LSemiColon 
  | LFor 
  | LWhile 
  | LContinue 
  | LBreak 
  | LNew 
  | LBy 
  | LReference 
  | LOp Text  
  | LVBar 
  | LIdentifier Text 
  | LComment Text
  | LEOF 
  deriving (Eq)

instance Show Token where 
  show (LChar t)       = "'" <> T.unpack t <> "'"
  show (LString t)     = "\"" <> T.unpack t <> "\""
  show (LNumber t)     = T.unpack t
  show (LAtom t)       = "#" <> T.unpack t
  show LOBckt          = "["
  show LCBckt          = "]"
  show LOBrc           = "{\n"
  show LCBrc           = "\n}"
  show LOParen         = "("
  show LCParen         = ")"
  show LComma          = ", "
  show LColon          = " : "
  show LFatArrow       = " => "
  show LAssign         = " := "
  show LMatch          = "match"
  show LWith           = "with"
  show LType           = "type"
  show LSemiColon      = ";\n"
  show LFor            = "for"
  show LWhile          = "while"
  show LContinue       = "continue"
  show LBreak          = "bre{-# LANGUAGE PatternSynonyms #-}ak"
  show LNew            = "new"
  show LBy             = "by"
  show LReference      = "reference"
  show (LOp t)         = T.unpack t
  show LVBar           = "|"
  show (LIdentifier t) = T.unpack t
  show (LComment t)    = "/*\n" <> T.unpack t <> "\n*/"
  show (LEOF)          = "\\EOF"

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

