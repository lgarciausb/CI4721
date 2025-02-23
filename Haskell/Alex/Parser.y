{
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
module Parser.Parser where 

import Parser.Lexer
import Parser.LexerDefinitions
import Data.Text (Text)
import Data.Text qualified as T
}


%name parse
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
  NEG        {LOp "-" _}
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

args : e {undefined}
     | e ',' args {undefined}

records : identifier ':' T mRecords {undefined}

mRecords : {- empty -} {undefined}
         | ',' records {undefined} 

T0 : atom {undefined}
  | identifier {undefined}
  | '{' records '}' {undefined}
  
T : T0 {undefined}
  | T '|' T0 {undefined}

moreArgs : ',' fun_args {undefined}
         | {- empty -} {undefined}

fun_args : T identifier moreArgs {undefined}

loop_a : break ';'  {undefined}
       | continue ';'  {undefined}
       | actions loop_as {undefined}

loop_as : {- empty -} {undefined}
        | loop_a {undefined}

optionalByRef : by reference {undefined}
              | {- empty -} {undefined}

pattern : number {undefined} 
         | char {undefined}
         | string {undefined}
         | char {undefined}
         | identifier optionalByRef {undefined}
         | '{' records '}' {undefined}

function_return : identifier {undefined}

patterns : pattern '=>' '{' actions '}' {undefined}
         | pattern '=>' '{' actions '}' patterns {undefined}

a0 : lvaluable ':=' e {undefined}
   | for '(' patterns ':' e ')' '{' loop_as '}' {undefined}
   | while '(' e ')' '{' loop_as '}' {undefined}
   | e {undefined}
   | identifier e {undefined}

as : as ';' a0 {undefined}
   | a0        {undefined}

actions : as ';' {undefined}
        | {-empty-} {undefined}

function_def : function_return identifier '(' fun_args ')' '{' actions '}' {undefined}

function_defs : function_defs function_def {undefined}
              | function_def {undefined}


e : identifier {undefined}
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

{
parseError :: Token -> Alex a
parseError _ = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

}
