{
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lexer where

import Control.Monad.State
import Control.Monad
import Data.Text (Text)
import Data.Word (Word8)
import Codec.Binary.UTF8.String (encode)
import Data.Text qualified as T
import Control.Lens
import Control.Lens.TH
import LexerDefinitions
import Data.Functor
}

%wrapper "monadUserState-strict-text"

$digit = 0-9
@operators = "+" | "-" | "*" | "/" | "^" | "%" | "<" | ">" | "!=" | "==" | ">=" | "<=" | " ||" | "&&" | "~" | "&"
@l_id = (a-z | A-Z | "_")(a-z | A-Z | "_" | 0-9)*

tokens :-
  <0>  $white+ ;
  <0> "'" ("\\'" | ~') "'"   {token $ \(_,_,_,s) _ -> LChar s       }
  <0> $digit+ ("." $digit+ | [e E] ("+" | "-")? $digit+)? {token $ \(_,_,_,s) _ -> LNumber s }
  <0> "#" l_id       {token $ \(_,_,_,s) _ -> LAtom $ T.tail $ s                       }
  <0> "["            {token $ \ _ _ -> LOBckt                                           }
  <0> "]"            {token $ \ _ _ -> LCBckt                                           }
  <0> "{"            {token $ \ _ _ -> LOBrc                                            }
  <0> "}"            {token $ \ _ _ -> LCBrc                                            }
  <0> "("            {token $ \ _ _ -> LOParen                                          }
  <0> ")"            {token $ \ _ _ -> LCParen                                          }
  <0> ","            {token $ \ _ _ -> LComma                                           }
  <0> ":"            {token $ \ _ _ -> LColon                                           }
  <0> "=>"           {token $ \ _ _ -> LFatArrow                                        }
  <0> ":="           {token $ \ _ _ -> LAssign                                          }
  <0> "match"        {token $ \ _ _ -> LMatch                                           }
  <0> "with"         {token $ \ _ _ -> LWith                                            }
  <0> "type"         {token $ \ _ _ -> LType                                            }
  <0> ";"            {token $ \ _ _ -> LSemiColon                                       }
  <0> "for"          {token $ \ _ _ -> LFor                                             }
  <0> "while"        {token $ \ _ _ -> LWhile                                           }
  <0> "continue"     {token $ \ _ _ -> LContinue                                        }
  <0> "break"        {token $ \ _ _ -> LBreak                                           }
  <0> "new"          {token $ \ _ _ -> LNew                                             }
  <0> "by"           {token $ \ _ _ -> LBy                                              }
  <0> "reference"    {token $ \ _ _ -> LReference                                       }
  <0> @operators     {token $ \(_,_,_,s) _ -> LOp s                                    }
  <0> "|"            {token $ \_ _ -> LVBar                                            }
  <0> @l_id          {token $ \(_,_,_,s) _ -> LIdentifier s                            } 
  <0,commentS> "/*"  { beginComment                                            }
  <commentS> "*/"    { endComment                                              }
  <commentS> [.\n]   { appendComment                                           }
  <0> "\""           { begin stringS                                             }
  <stringS> "\""     { endString                                               }
  <stringS> \\[nt\"] { escapeString                                            }
  <stringS> .        { appendString                                            }


{

instance MonadState AlexState Alex where 
  state f = Alex $ \s -> Right $ (\(a,b) -> (b,a)) $ f s

ifThenElse :: Bool -> a -> a -> a
ifThenElse t a b = case t of 
  True -> a
  False -> b

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
endString _ _ = do 
 buf <- _austTextBuff <$> alexGetUserState
 alexModifyUserState (\s -> s{_austTextBuff = mempty})
 pure $ LString (T.reverse buf)

beginComment :: AlexAction Token 
beginComment i _ = alexModifyUserState (\s 
   -> s{_austCommentDepth = s._austCommentDepth + 1}
  ) *> skip i 1

endComment :: AlexAction Token 
endComment i _ = do
  alexModifyUserState (\s -> s{_austCommentDepth = s._austCommentDepth - 1})
  modify (\s -> s{alex_scd = if s.alex_scd == 1 then 0 else commentS})
  s <- alexGetUserState
  case _austCommentDepth s of 
    0 -> pure $ LString $ T.reverse $ _austCommentBuff s
    _ -> skip i 1

appendComment :: AlexAction Token
appendComment  i@(_,_,_,(_ T.:< c T.:< _)) _ = alexModifyUserState (\s 
  -> s{_austCommentBuff  =  c T.:< s._austCommentBuff}
 ) *> skip i 1


}
