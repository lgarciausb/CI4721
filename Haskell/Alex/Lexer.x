{
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Parser.Lexer where

import Control.Monad.State
import Control.Monad
import Data.Text (Text)
import Data.Word (Word8)
import Codec.Binary.UTF8.String (encode)
import Data.Text qualified as T
import Control.Lens
import Control.Lens.TH
import Parser.LexerDefinitions
import Data.Functor
}

%wrapper "monadUserState-strict-text"

$digit = [0-9]
$alpha = [a-zA-Z]
@operators = "+" | "-" | "*" | "/" | "^" | "%" | "<" | ">" | "!=" | "==" | ">=" | "<=" | " ||" | "&&" | "~" | "&"
@id = ( $alpha | \_) ($alpha | \_ | digit)*

tokens :-
  <0>  $white+ ;
  <0> "'" ("\\'" | ~') "'"   {token $ \(pos,_,_,s) l -> LChar (T.take l s) pos                     }
  <0> $digit+ ("." $digit+ | [e E] ("+" | "-")? $digit+)? {token $ \(pos,_,_,s) l -> LNumber (T.take l s) pos }
  <0> "#" id         {token $ \(pos,_,_,s) l -> LAtom (T.tail $ T.take l s) pos                    }
  <0> "["            {token $ \ (pos,_,_,_) _ -> LOBckt pos                                               }
  <0> "]"            {token $ \ (pos,_,_,_) _ -> LCBckt pos                                              }
  <0> "{"            {token $ \ (pos,_,_,_) _ -> LOBrc  pos                                               }
  <0> "}"            {token $ \ (pos,_,_,_) _ -> LCBrc  pos                                               }
  <0> "("            {token $ \ (pos,_,_,_) _ -> LOParen pos                                              }
  <0> ")"            {token $ \ (pos,_,_,_) _ -> LCParen pos                                              }
  <0> ","            {token $ \ (pos,_,_,_) _ -> LComma  pos                                              }
  <0> ":"            {token $ \ (pos,_,_,_) _ -> LColon  pos                                              }
  <0> "=>"           {token $ \ (pos,_,_,_) _ -> LFatArrow pos                                            }
  <0> ":="           {token $ \ (pos,_,_,_) _ -> LAssign   pos                                            }
  <0> "match"        {token $ \ (pos,_,_,_) _ -> LMatch    pos                                            }
  <0> "with"         {token $ \ (pos,_,_,_) _ -> LWith     pos                                            }
  <0> "type"         {token $ \ (pos,_,_,_) _ -> LType     pos                                            }
  <0> ";"            {token $ \ (pos,_,_,_) _ -> LSemiColon  pos                                          }
  <0> "for"          {token $ \ (pos,_,_,_) _ -> LFor        pos                                          }
  <0> "while"        {token $ \ (pos,_,_,_) _ -> LWhile      pos                                          }
  <0> "continue"     {token $ \ (pos,_,_,_) _ -> LContinue   pos                                          }
  <0> "break"        {token $ \ (pos,_,_,_) _ -> LBreak      pos                                          }
  <0> "new"          {token $ \ (pos,_,_,_) _ -> LNew        pos                                          }
  <0> "by"           {token $ \ (pos,_,_,_) _ -> LBy         pos                                          }
  <0> "reference"    {token $ \ (pos,_,_,_) _ -> LReference  pos                                          }
  <0> @operators     {token $ \(pos,_,_,s) l -> LOp (T.take l s) pos                               }
  <0> "|"            {token $ \(pos,_,_,_) _ -> LVBar pos                                                   }
  <0> "."            {token $ \(pos,_,_,_) _ -> LDot pos                                                   }
  <0> @id            {token $ \(pos,_,_,s) l -> LIdentifier (T.take l s) pos                       } 
  <0,commentS> "/*"  { beginComment                                                          }
  <commentS> "*/"    { endComment                                                            }
  <commentS> [.\n]   { appendComment                                                         }
  <0> \"           { begin stringS                                                         }
  <stringS> \"     { endString                                                             }
  <stringS> \\[nt\"] { escapeString                                                          }
  <stringS> .        { appendString                                                          }


{

type Token = Token' AlexPosn

instance MonadState AlexState Alex where 
  state f = Alex $ \s -> Right $ (\(a,b) -> (b,a)) $ f s


alexEOF :: Alex Token 
alexEOF = pure LEOF

alexModifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
alexModifyUserState f = get >>= \s -> put $ s{alex_ust= f s.alex_ust}

appendString ::AlexAction Token 
appendString i@(_,_,_,(c T.:< cs)) _ = alexModifyUserState (\s 
  -> s{_austTextBuff  = cons c $ s._austTextBuff}
 ) *> skip i 1

escapeString :: AlexAction Token
escapeString i@(_,_,_,(_ T.:< c T.:< _)) _ = alexModifyUserState (\s 
  -> s{_austTextBuff = cons unesc $ s._austTextBuff}
 ) *> skip i 1
 where unesc = case c of
         'n' -> '\n'
         't' -> '\t'
         '"' -> '"'

endString :: AlexAction Token
endString (pos,_,_,_) _ = do 
 buf <- _austTextBuff <$> alexGetUserState
 alexModifyUserState (\s -> s{_austTextBuff = mempty})
 modify $ \s -> s{alex_scd=0}
 pure $ LString (T.reverse buf) pos

beginComment :: AlexAction Token 
beginComment i _ = alexModifyUserState (\s 
   -> s{_austCommentDepth = s._austCommentDepth + 1}
  ) *> skip i 1

endComment :: AlexAction Token 
endComment i@(pos,_,_,_) _ = do
  alexModifyUserState (\s -> s{_austCommentDepth = s._austCommentDepth - 1})
  modify (\s -> s{alex_scd = if s.alex_scd == 1 then 0 else commentS})
  s <- alexGetUserState
  case _austCommentDepth s of 
    0 -> pure $ LString (T.reverse $ _austCommentBuff s) pos
    _ -> skip i 1

appendComment :: AlexAction Token
appendComment  i@(_,_,_,(_ T.:< c T.:< _)) _ = alexModifyUserState (\s 
  -> s{_austCommentBuff  =  c T.:< s._austCommentBuff}
 ) *> skip i 1


scanMany :: Text -> Either String [Token]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if output == LEOF
        then pure [output]
        else (output :) <$> go

}
