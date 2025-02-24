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

ixs :: {[Expression AlexPosn]}
ixs : '[' e ']' {[$2]}
    | ixs '[' e ']' {$1 <> [$3]}

lvaluable :: {LValuable AlexPosn}
lvaluable : identifier ixs {$2 |> $1 |> \(LIdentifier t p) ixs -> PLIndexed t ixs p}
          | identifier '.' lvaluable {$3 |> $1 |> \(LIdentifier t p) l -> PLDot t l p}
          | identifier {$1 |> \(LIdentifier t p) -> PLId t p}

args :: {[Expression AlexPosn]}
args : e {[$1]}
     | args ',' e {$1 <> [$3]}
     | {-empty-} {[]}

records :: {RecordPattern AlexPosn}
records : identifier ':' T mRecords {(\(LIdentifier t p) ty rs -> RecordPattern(t,ty,p) rs p) $1 $3 $4}

mRecords :: {[(Text,PTypes AlexPosn,AlexPosn)]}
mRecords : {- empty -} {[]}
         | ',' records { (\(RecordPattern r rs _) -> r:rs) $2  } 

T0 :: {PTypes AlexPosn}
T0 : atom {$1 & \(LAtom t p) -> PAtom t p}
  | identifier { $1 & \(LIdentifier t p) -> PId t p}
  | '{' records '}' {PRecord $2}

T :: {PTypes AlexPosn}
T : T0 {$1}
  | T '|' T0 {PUnion $1 $3 }

loop_a0 :: {LoopAction AlexPosn}
loop_a0 : break  {$1 |> \(LBreak p) -> Break p}
       | continue {$1 |> \(LContinue p) -> Continue p}
       | a0 {LAction $1}

loop_a :: {[LoopAction AlexPosn]}
loop_a : loop_a ';' loop_a0 {$1 <> [$3]}
       | loop_a0 {[$1]} 

loop_as :: {[LoopAction AlexPosn]}
loop_as : loop_a ';'  {$1}

pattern :: {Pattern AlexPosn}
pattern : number {$1 |> \(LNumber t p) -> PaNumber t p} 
         | char {$1 |> \(LChar t p) -> PaChar t p}
         | string {$1 |> \(LString t p) -> PaString t p}
         | identifier optionalByRef {$2 |> $1 |> \(LIdentifier t p) mref -> PaId t mref p}
         | '{' records '}' {PaPattern $2}

patterns :: {[(Pattern AlexPosn,[Action AlexPosn])]}
patterns : pattern '=>' '{' actions '}' {[($1,$4)]}
         | patterns pattern '=>' '{' actions '}' {$1 <> [($2,$5)]}

a0 :: {Action AlexPosn}
a0 : e {AExpression $1}
   | for '(' pattern ':' e ')' '{' loop_as '}'  { $8 |> $5 |> $3 |> $1 |>
                                                  \(LFor p) pt e as -> For pt e as p
                                                }
   | while '(' e ')' '{' loop_as '}'  { $6 |> $3 |> $1 |> 
                                        \(LWhile p) e as -> While e as p
                                      }
   | lvaluable ':=' e {Assign $1 $3}

as :: {[Action AlexPosn]}
as : as ';' a0 {$1 <> [$3]}
   | a0 {[$1]}

actions :: {[Action AlexPosn]}
actions : as ';' {$1}
        | {-empty-} {[]}

optionalByRef :: {Maybe (ByRef AlexPosn)}
optionalByRef : by reference {$1 & \(LBy p) -> Just $ ByRef p  }
              | {- empty -} {Nothing}

e :: {Expression AlexPosn}
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


fun_args :: {FunArgs AlexPosn}
fun_args  : T identifier optionalByRef  ',' fun_args { $5 |> $3 |> $2 |> $1 |>
                                                       \t (LIdentifier x _) mref fs 
                                                        -> FunArg t x mref (getPTypesInfo t) : fs
                                                     }
          | {-empty-} {[]}

function_def :: {FunctionDef AlexPosn}
function_def : T '(' fun_args ')' '{' actions '}' { $6 |> $3 |> $1 |>
                                                    \t fs as 
                                                      -> FunctionDef t fs as (getPTypesInfo t) 
                                                  }
function_defs :: {[FunctionDef AlexPosn]}
function_defs : function_defs function_def {$1 <> [$2]}
              | function_def {[$1]}

{
parseError :: Token -> Alex a
parseError _ = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

}
