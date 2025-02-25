{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
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
  deriving (Show,Eq,Functor,Foldable)



data RecordPattern a 
  = RecordPattern  (Text, PTypes a, a) [(Text,PTypes a, a)] a
  deriving (Eq,Show)


data PTypes a 
  = PAtom Text a
  | PId Text a
  | PUnion (PTypes a) (PTypes a) 
  | PRecord (RecordPattern a) 
  deriving (Eq,Show)

getPTypesInfo :: PTypes a -> a
getPTypesInfo (PAtom _ a) = a
getPTypesInfo (PId _ a) = a
getPTypesInfo (PUnion a _) = getPTypesInfo a
getPTypesInfo (PRecord (RecordPattern _ _ a)) = a

data ByRef a = ByRef a deriving (Eq,Show)

data FunArg a = FunArg (PTypes a) Text (Maybe (ByRef a)) a deriving (Eq,Show)

type FunArgs a = [FunArg a]

data FunctionDef a = FunctionDef (PTypes a) Text [FunArg a] [Action a] a deriving (Eq,Show)

data LValuable a 
  = PLId Text a 
  | PLIndexed Text [Expression a] a
  | PLDot Text (LValuable a) a
  deriving (Eq,Show)

getLValueInfo :: LValuable a -> a
getLValueInfo (PLId _ a) = a
getLValueInfo (PLIndexed _ _ a) = a
getLValueInfo (PLDot _ _ a) = a

data LoopAction a 
  = Break a
  | Continue a 
  | LAction (Action a)
  deriving (Eq,Show)

data Pattern a
  = PaNumber Text a
  | PaString Text a
  | PaChar Text a
  | PaId Text (Maybe (ByRef a)) a
  | PaPattern (RecordPattern a) 
  deriving (Eq,Show)

data Action a
  = For (Pattern a) (Expression a) [LoopAction a] a
  | While (Expression a) [LoopAction a] a
  | Assign (LValuable a) (Expression a)
  | AExpression (Expression a)
  | Return (Expression a) a
  deriving (Eq,Show)


data Expression a  
  = ELValuable (LValuable a)
  | New (PTypes a) [Expression a] a
  | Ref (Expression a) a
  | Neg (Expression a) a
  | Plus (Expression a) (Expression a) a
  | Times (Expression a) (Expression a) a
  | Divide (Expression a) (Expression a) a
  | Power (Expression a) (Expression a) a
  | Mod (Expression a) (Expression a) a
  | Minus (Expression a) (Expression a) a
  | ELT (Expression a) (Expression a) a
  | EGT (Expression a) (Expression a) a
  | NotEq (Expression a) (Expression a) a
  | EEq (Expression a) (Expression a) a
  | EGTEq (Expression a) (Expression a) a
  | ELTEq (Expression a) (Expression a) a
  | Or (Expression a) (Expression a) a
  | And (Expression a) (Expression a) a
  | Not (Expression a) a
  | Arr [Expression a] a
  | ENumber Text a
  | EString Text a 
  | EChar Text a
  | Match (Expression a) [(Pattern a,[Action a])] a
  | FApp Text [Expression a] a
  deriving (Eq,Show)

getExpressionInfo :: Expression a -> a
getExpressionInfo (ELValuable a) = getLValueInfo a
getExpressionInfo (New _ _ a) = a
getExpressionInfo (Ref _ a) = a
getExpressionInfo (Neg _ a) = a
getExpressionInfo (Plus _ _ a) = a
getExpressionInfo (Times _ _ a) = a
getExpressionInfo (Divide _ _ a) = a
getExpressionInfo (Power _ _ a) = a
getExpressionInfo (Mod _ _ a) = a
getExpressionInfo (Minus _ _ a) = a
getExpressionInfo (ELT _ _ a) = a
getExpressionInfo (EGT _ _ a) = a
getExpressionInfo (NotEq _ _ a) = a
getExpressionInfo (EEq _ _ a) = a
getExpressionInfo (EGTEq _ _ a) = a
getExpressionInfo (ELTEq _ _ a) = a
getExpressionInfo (Or _ _ a) = a
getExpressionInfo (And _ _ a) = a
getExpressionInfo (Not _ a) = a
getExpressionInfo (Arr _ a) = a
getExpressionInfo (ENumber _ a) = a
getExpressionInfo (EString _ a) = a
getExpressionInfo (EChar _ a) = a
getExpressionInfo (Match _ _ a) = a
getExpressionInfo (FApp _ _ a) = a














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

