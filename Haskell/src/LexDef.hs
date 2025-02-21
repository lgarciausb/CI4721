{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
module LexDef where 


import Control.Monad.State
import Control.Monad
import Data.Text (Text)
import Data.Word (Word8)
import Codec.Binary.UTF8.String (encode)
import Data.Text qualified as T
import Control.Lens
import Control.Lens.TH 
import Data.Functor

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

data AlexPosn = AlexPn
  { _apCharOffset :: !Int            -- ^ absolute character offset
  , _apLino       :: !Int            -- ^ line number
  , _apColno      :: !Int             -- ^ column number
  }
data AlexInput = AlexInput
  { _aiCurrentPos  :: !AlexPosn -- ^ current position,
  , _aiPrevChar    :: Char     -- ^ previous char
  , _aiBytes       :: [Word8]  -- ^ rest of the bytes for the current char
  , _aiRest        :: Text
  }

$(makeLenses ''AlexPosn)
$(makeLenses ''AlexInput)

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte ai = case (_aiBytes ai,_aiRest ai) of
  ((b:bs),_)   -> Just (b,ai{_aiBytes=bs})
  ([],T.Empty) -> Nothing
  ([],c T.:< cs) -> let 
    oldLineno = ai ^. aiCurrentPos  . apLino 
    oldColno  = ai ^. aiCurrentPos  . apColno 
    newLineno = if c == '\n' then oldLineno + 1 else oldLineno 
    newColno  = if c == '\n' then 1 else oldColno + 1
    newPos    = AlexPn {_apCharOffset = (_apCharOffset . _aiCurrentPos) ai + 1, _apLino = newLineno, _apColno = newColno}
    (b:bs) = encode [c]
    in Just (b,AlexInput 
      { _aiCurrentPos = newPos 
      , _aiPrevChar = c
      , _aiBytes = bs 
      , _aiRest  = cs 
      })
    
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput {_aiPrevChar=c}) = c

data AlexUserState = AUST 
  { _austCommentDepth :: !Int
  , _lexSC            :: !Int
  , _austTextBuff     :: Text 
  }

data AlexState = AlexState
  { _input   :: !AlexInput
  , _astUst  :: !AlexUserState  -- AlexUserState will be defined in the user program
  }

$(makeLenses ''AlexUserState)
$(makeLenses ''AlexState)


initialState:: Text -> AlexState
initialState s = AlexState 
  { _input = AlexInput 
      { _aiCurrentPos =  AlexPn {_apCharOffset = 1, _apLino = 1, _apColno = 1}
      , _aiPrevChar   = '\n'
      , _aiBytes      = []
      , _aiRest       = s
      }
  , _astUst = AUST 
      { _austCommentDepth = 0
      , _lexSC            = 0
      , _austTextBuff     = mempty
      }
  }

type P a = StateT AlexState (Either Text) a

getLineNo :: P Int
getLineNo = get <&> \s -> s ^. input . aiCurrentPos . apLino 

getColNo :: P Int
getColNo = get <&> \s -> s ^. input . aiCurrentPos . apColno

evalP::P a -> Text -> Either Text a
evalP m s= evalStateT m (initialState s)



-- type LexAction = Int -> Text -> P (Maybe Token)
--
--
-- type LexAction = Int -> Text -> P (Maybe Token)
--
-- plainTok:: Token -> LexAction
-- plainTok t _ _ = return (Just t)
--
-- textTok :: (Text -> Token) -> LexAction
-- textTok cons _ s = return $ Just (cons s)
--
-- type LexAction = Int -> Text -> P (Maybe Token)
--
--
-- beginString :: LexAction
-- beginString _ _ = modify $ \s -> s & astUst . lexSC .~ stringS 
--
-- appendString ::LexAction
-- appendString _ (c T.:< _) = (<$) Nothing . modify $ \s 
--   -> s & astUst . austTextBuff  %~ cons c 
-- appendString _ _ = pure Nothing
--
-- escapeString :: LexAction
-- escapeString _ (_ T.:< c T.:< _) = (<$) Nothing . modify $ \s 
--   -> s & astUst . austTextBuff  %~ cons unesc 
--   where unesc = case c of
--           'n' -> '\n'
--           't' -> '\t'
--           '"' -> '"'
-- escapeString _ _ = pure Nothing
--
--
-- endString :: LexAction
-- endString _ _ = do
--   s <- get
--   let buf = s ^. astUst . austTextBuff 
--   put $ s & astUst . lexSC .~ 0 
--           & astUst . austTextBuff .~ mempty  
--   return $ Just $ LString (T.reverse buf)
--
-- beginComment :: LexAction
-- beginComment _ _ = (<$) Nothing . modify $ \s 
--   -> s  & astUst . lexSC  .~ commentS 
--         & astUst . austCommentDepth  %~ (+1)
--
-- endComment :: LexAction 
-- endComment _ _ = do
--   s <- get
--   put $ s & astUst . lexSC %~ (\cd -> if cd == 1 then 0 else commentS) 
--           & astUst . austCommentDepth %~ subtract 1   
--   return Nothing
--
-- readToken :: P Token
-- readToken = do
--   s <- get
--   case alexScan (s ^. input) (s ^. astUst . lexSC) of
--     AlexEOF -> pure LEOF
--     AlexError inp' -> lift . Left $ 
--       "Lexical error on line " <> (inp' ^. aiCurrentPos . apLino & T.show) <> 
--       ", column: " <> (inp' ^. aiCurrentPos . apColno & T.show)
--     AlexSkip inp' _ -> modify (\s' -> s' & input .~ inp') *> readToken
--     AlexToken inp' n act -> do 
--       let buf = s ^. input . aiRest 
--       put $ s & input .~ inp'
--       res <- act n (T.take n buf)
--       case res of
--         Nothing -> readToken
--         Just t -> pure t
--
-- lexer ::(Token -> P a) -> P a
-- lexer cont = do
--   tok <- readToken
--   cont tok
--
-- alexScan :: AlexInput -> Int -> AlexReturn (AlexPosn -> Text -> P (Maybe Token))
-- alexScan = undefined
--
-- data AlexReturn action
--   = AlexEOF
--
--   | AlexError
--       !AlexInput     -- Remaining input
--
--   | AlexSkip
--       !AlexInput     -- Remaining input
--       !Int           -- Token length
--
--   | AlexToken
--       !AlexInput     -- Remaining input
--       !Int           -- Token length
--       action         -- action value
--
--
-- commentS = 0
--
-- stringS = 0
