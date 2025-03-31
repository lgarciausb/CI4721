{
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
module Parser.Parser where 

import Parser.Lexer
import Parser.LexerDefinitions
import Data.Text (Text)
import Data.Text qualified as T
import Control.Lens ((&))
}


%name parse definitions
%tokentype { Token }
%error { parseError }
%monad { Alex } { >>= } { pure }
%lexer { lexer } {LEOF}



%token 
  charlit    {LCharLit _ _}
  stringlit  {LStringLit _ _}
  boollit  {LBoolLit _ _}
  number     {LNumber _ _}
  atom       {LAtom _ _}
  bool       {LBool _}
  int        {LInt _}
  float      {LFloat _}
  char       {LChar _}
  string     {LString _}
  unit       {LUnit _}
  void       {LVoid _}
  vector     {LVector _}
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
  '|'        {LVBar _}
  identifier {LIdentifier _ _}
  '.'        {LDot _}
  EOF        {LEOF}
  '+'        {LPlus _}
  '-'        {LMinus _}
  '*'        {LMult _}
  '/'        {LDiv _}
  '^'        {LPow _}
  '%'        {LMod _}
  '<'        {LLT _}
  '>'        {LGT _}
  '!='       {LNEq _}
  '=='       {LEq _}
  '>='       {LGTE _}
  '<='       {LLTE _}
  '||'       {LOr _}
  '&&'       {LAnd _}
  '~'        {LNot  _}
  '&'        {LRef _}
  return    {LReturn _}

%nonassoc return
%left ','
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
  | bool {$1 & \(LBool p) -> PBool p}
  | int {$1 & \(LInt p) -> PInt p}
  | float {$1 & \(LFloat p) -> PFloat p}
  | char {$1 & \(LChar p) -> PChar p}
  | string {$1 & \(LString p) -> PString p}
  | unit {$1 & \(LUnit p) -> PUnit p}
  | void {$1 & \(LVoid p) -> PVoid p}
  | vector '<' T '>' {$3 |> $1 |> \(LVector p) ty -> PVector ty p}
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
         | charlit {$1 |> \(LCharLit t p) -> PaChar t p}
         | stringlit {$1 |> \(LStringLit t p) -> PaString t p}
         | boollit {$1 |> \(LBoolLit t p) -> PaBool t p}
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
   | T identifier ':=' e { $4 |> $2 |> $1 |> \ty (LIdentifier t _) e -> Declare ty t (Just e)}
   | T identifier { $2 |> $1 |> \ty (LIdentifier t _) -> Declare ty t Nothing}
   | return e {$1 |> \(LReturn p) -> Return $2 p} 
   | return unit {$2 |> $1 |> \(LReturn p) (LUnit p') -> Return (Unit p') p} 
 

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
e : lvaluable { ELValuable $1 }
  | new T '(' args ')' {$1 |> \(LNew p) -> New $2 $4 p}
  | '&' e {$1 |> \(LRef p) -> Ref $2 p}
  | '-' e %prec NEG {$1 |> \(LMinus p) -> Neg $2 p}
  | e '+' e {Plus $1 $3 (getExpressionInfo $1)}
  | e '*' e {Times $1 $3 (getExpressionInfo $1)}
  | e '/' e {Divide $1 $3 (getExpressionInfo $1)}
  | e '^' e {Power $1 $3 (getExpressionInfo $1)}
  | e '%' e {Mod $1 $3 (getExpressionInfo $1)}
  | e '-' e {Minus $1 $3 (getExpressionInfo $1)}
  | e '<' e {ELT $1 $3 (getExpressionInfo $1)}
  | e '>' e {EGT $1 $3 (getExpressionInfo $1)}
  | e '!='e {NotEq $1 $3 (getExpressionInfo $1)}
  | e '==' e {EEq $1 $3 (getExpressionInfo $1)}
  | e '>=' e {EGTEq $1 $3 (getExpressionInfo $1)}
  | e '<=' e {ELTEq $1 $3 (getExpressionInfo $1)}
  | e '||' e {Or $1 $3 (getExpressionInfo $1)}
  | e '&&' e {And $1 $3 (getExpressionInfo $1)}
  | '~' e {$1 |> \(LNot p) -> Not $2 p }
  | '(' e ')' {$2}
  | '[' args ']' {$1 |> \(LOBckt p) -> Arr $2 p}
  | number {$1 |> \(LNumber t p) -> ENumber t p}
  | stringlit {$1 |> \(LStringLit t p) -> EString t p}
  | charlit {$1 |> \(LCharLit t p) -> EChar t p}
  | boollit {$1 |> \(LBoolLit t p) -> EBool t p}
  | match e with patterns {$1 |> \(LMatch p) -> Match $2 $4 p}
  | identifier '(' args ')' {$1 |> \(LIdentifier t p) -> FApp t $3 p}


fun_params :: {FunArgs AlexPosn}
fun_params  : fun_params ',' fun_params { $1 <> $3 }  
          | T identifier optionalByRef {$3 |> $2 |> $1 |> \t (LIdentifier x _) mref -> [FunArg t x mref $ getPTypesInfo t]}

m_fun_params :: {[FunArg AlexPosn]}
m_fun_params : fun_params {$1}
           | {-empty-} {[]}

definition :: {Definition AlexPosn}
definition : T identifier '(' m_fun_params ')' '{' actions '}' { $7 |> $4 |> $2 |> $1 |>
                                                    \t (LIdentifier name _) fs as 
                                                      -> FunctionDef t name fs as (getPTypesInfo t) 
                                                  }
           | type identifier ':=' T { $4 |> $2 |> $1 |> \(LType p) (LIdentifier t _) ty -> TypeDef t ty p} 
           
definitions :: {[Definition AlexPosn]}
definitions :  definitions definition {$1 <> [$2]}
            | definition {[$1]}


{
parseError :: Token -> Alex a
parseError _ = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (Token -> Alex a) -> Alex a
lexer f = alexMonadScan >>= \case 
  LComment {} -> lexer f
  t -> f t

}
