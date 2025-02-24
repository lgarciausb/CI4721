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

infixr 1 |>
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

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

data RecordPattern a 
  = RecordPattern  (Text, PTypes a, a) [(Text,PTypes a, a)] a


data PTypes a 
  = PAtom Text a
  | PId Text a
  | PUnion (PTypes a) (PTypes a) 
  | PRecord (RecordPattern a) 

getPTypesInfo :: PTypes a -> a
getPTypesInfo (PAtom _ a) = a
getPTypesInfo (PId _ a) = a
getPTypesInfo (PUnion a _) = getPTypesInfo a
getPTypesInfo (PRecord (RecordPattern _ _ a)) = a

data ByRef a = ByRef a

data FunArg a = FunArg (PTypes a) Text (Maybe (ByRef a)) a

type FunArgs a = [FunArg a]

data FunctionDef a = FunctionDef (PTypes a) [FunArg a] [Action a] a

data LValuable a 
  = PLId Text a 
  | PLIndexed Text [Expression a] a
  | PLDot Text (LValuable a) a

data LoopAction a 
  = Break a
  | Continue a 
  | LAction (Action a)

data Pattern a
  = PaNumber Text a
  | PaString Text a
  | PaChar Text a
  | PaId Text (Maybe (ByRef a)) a
  | PaPattern (RecordPattern a) 

data Action a
  = For (Pattern a) (Expression a) [LoopAction a] a
  | While (Expression a) [LoopAction a] a
  | Assign (LValuable a) (Expression a)
  | AExpression (Expression a)


data Expression a  

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

