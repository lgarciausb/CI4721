{
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Parser.Parser where 

import Parser.Lexer
import Parser.LexerDefinitions
import Data.Text (Text)
import Data.Text qualified as T
import Control.Lens ((&))
}


%name parse function_defs
%tokentype { Token }
%error { parseError }
%monad { Alex } { >>= } { pure }
%lexer { lexer } {LEOF}



%token 
  char       {LChar _ _}
  string     {LString _ _}
  number     {LNumber _ _}
  atom       {LAtom _ _}
  '['        {LOBckt _}
  ']'        {LCBckt _}
  '{'        {LOBrc _}
  '}'        {LCBrc _}
  '('        {LOParen _}
  ')'        {LCParen _}
  ','        {LComma _}
  ':'        {LColon _}
  '=>'       {LFatArrow _}
  ':='       {LAssign _}
  match      {LMatch _}
  with       {LWith _}
  type       {LType _}
  ';'        {LSemiColon _}
  for        {LFor _}
  while      {LWhile _}
  continue   {LContinue _}
  break      {LBreak _}
  new        {LNew _}
  by         {LBy _}
  reference  {LReference _}
  '***'      {LOp _ _}
  '|'        {LVBar _}
  identifier {LIdentifier _ _}
  '.'        {LDot _}
  EOF        {LEOF}
  '+'        {LOp "+" _}
  '-'        {LOp "-" _}
  '*'        {LOp "*" _}
  '/'        {LOp "/" _}
  '^'        {LOp "^" _}
  '%'        {LOp "%" _}
  '<'        {LOp "<" _}
  '>'        {LOp ">" _}
  '!='       {LOp "!=" _}
  '=='       {LOp "==" _}
  '>='       {LOp ">=" _}
  '<='       {LOp "<=" _}
  '||'       {LOp "||" _}
  '&&'       {LOp "&&" _}
  '~'        {LOp "~"  _}
  '&'        {LOp "&" _}

%right '||' 
%right '&&'
%left '<' '>' '==' '!=' '>=' '<=' 
%left '+' '-'
%left '*' '/' '%'
%right '^' 
%left NEG '&' '~'

%%

ixs : '[' e ']' {undefined}
    | ixs '[' e ']' {undefined}

lvaluable : identifier ixs {undefined}
          | identifier '.' lvaluable {undefined}
          | identifier {undefined}

args : e {undefined}
     | e ',' args {undefined}

records :: {PTypes AlexPosn}
records : identifier ':' T mRecords {(\(LIdentifier t p) ty rs -> PRecord (t,ty,p) rs p) $1 $3 $4}

mRecords :: {[(Text,PTypes AlexPosn,AlexPosn)]}
mRecords : {- empty -} {[]}
         | ',' records { (\(PRecord r rs _) -> r:rs) $2  } 

T0 : atom {$1 & \(LAtom t p) -> PAtom t p}
  | identifier { $1 & \(LIdentifier t p) -> PId t p}
  | '{' records '}' {$2}
  
T : T0 {$1}
  | T '|' T0 {PUnion $1 $3 }

loop_a0 : break  {undefined}
       | continue {undefined}
       | a0 {undefined}

loop_a : loop_a ';' loop_a0 {undefined}
       | loop_a0 {undefined} 

loop_as : loop_a ';'  {undefined}

pattern : number {undefined} 
         | char {undefined}
         | string {undefined}
         | identifier optionalByRef {undefined}
         | '{' records '}' {undefined}

patterns : pattern '=>' '{' actions '}' {undefined}
         | patterns pattern '=>' '{' actions '}' {undefined}

a0 : e {undefined}
   | for '(' patterns ':' e ')' '{' loop_as '}' {undefined}
   | while '(' e ')' '{' loop_as '}' {undefined}
   | lvaluable ':=' e {undefined}


as : as ';' a0 {undefined}
   | a0 {undefined}

actions : as ';' {undefined}
        | {-empty-} {undefined}

optionalByRef : by reference {undefined}
              | {- empty -} {undefined}


e : lvaluable {undefined}
  | new T '(' args ')' {undefined}
  | '&' lvaluable {undefined}
  | '-' e %prec NEG {undefined}
  | e '+' e {undefined}
  | e '*' e {undefined}
  | e '/' e {undefined}
  | e '^' e {undefined}
  | e '%' e {undefined}
  | e '-' e {undefined}
  | e '<' e {undefined}
  | e '>' e {undefined}
  | e '!='e {undefined}
  | e '==' e {undefined}
  | e '>=' e {undefined}
  | e '<=' e {undefined}
  | e '||' e {undefined}
  | e '&&' e {undefined}
  | '~' e {undefined}
  | '(' e ')' {undefined}
  | '[' args ']' {undefined}
  | number {undefined}
  | string {undefined}
  | char {undefined}
  | match e with patterns {undefined}
  | identifier '(' args ')' {undefined}


moreArgs : ',' fun_args {undefined}
         | {- empty -} {undefined}

fun_args : T identifier optionalByRef  moreArgs {undefined}


function_def : T '(' fun_args ')' '{' actions '}' {undefined}

function_defs : function_defs function_def {undefined}
              | function_def {undefined}

{
parseError :: Token -> Alex a
parseError _ = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

}
