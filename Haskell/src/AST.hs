{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE TypeAbstractions         #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ImportQualifiedPost      #-}
module AST where 


import Data.Kind (Type, Constraint)
import Types
import Data.Text (Text)
import Data.List.NonEmpty
import Prelude.Singletons
import Data.Singletons.TH 
import Data.List.Singletons 
import Data.Text qualified as T

$(singletons [d|
  data ActionCtx 
    = LoopCtx 
    | ReturnCtx
    | PureCtx

  data PassOption 
    = POValue 
    | PORef

  |])

defineActionCtx :: [ActionCtx] -> [ActionCtx] -> [ActionCtx]
defineActionCtx [ReturnCtx] _ = [ReturnCtx]
defineActionCtx _ [ReturnCtx] = [ReturnCtx]
defineActionCtx [PureCtx] a = a
defineActionCtx a [PureCtx] = a 
defineActionCtx _ b = b



data Pattern (ctx :: Type) (a :: PTypes) where 
  PaZ      :: PaZX ctx   -> Int    -> Pattern ctx PZ 
  PaF      :: PaFX ctx   -> Float  -> Pattern ctx PF 
  PaB      :: PaBX ctx   -> Bool   -> Pattern ctx PZBool 
  PaChar   :: PaCharX ctx -> Char   -> Pattern ctx PZChar 
  PaString :: PaStringX ctx -> Text -> Pattern ctx PZString 
  PaId     :: SingI a => PaIdX ctx a -> Text   -> PassOption -> Pattern ctx a 
  PaRecord :: forall a ctx. 
    (SingI a, IsRecord a ~ True)
    => PaRecordX ctx a -> Demote (KindOf a) -> Pattern ctx a
  PaErr    :: SingI a => PaErrX ctx a -> Pattern ctx a  

type family PaZX      (ctx :: Type) :: Type 
type family PaFX      (ctx :: Type) :: Type 
type family PaBX      (ctx :: Type) :: Type 
type family PaCharX   (ctx :: Type) :: Type 
type family PaStringX (ctx :: Type) :: Type 
type family PaIdX     (ctx :: Type) (a :: PTypes) :: Type 
type family PaErrX    (ctx :: Type) (a :: PTypes) :: Type 
type family PaRecordX (ctx :: Type) (a :: PTypes) :: Type

data Accessors (ctx :: Type) (a :: PTypes)  where 
  AccSimple :: forall a b ctx. 
    (SingI a, SingI b) => AccSimpleX ctx a -> Text -> Accessors ctx (a :~> b)
  AccDot :: forall a b c ctx.
    (SingI a, SingI b, SingI c) 
    => AccDotX ctx a b c -> Accessors ctx (a :~> b) -> Accessors ctx (b :~> c)  -> Accessors ctx (a :~> c)
  AccArr  :: forall a b ctx.
    (SingI a, SingI b)
    => AccArrX ctx a b 
    -> Accessors ctx (a :~> PZArray b) -> E ctx PZ -> Accessors ctx (a :~> b)

type family AccSimpleX (ctx :: Type) (a :: PTypes) :: Type 
type family AccDotX (ctx :: Type) (a :: PTypes) (b :: PTypes) (c :: PTypes) :: Type 
type family AccArrX (ctx :: Type) (a :: PTypes) (b :: PTypes) :: Type

data LValuable (ctx :: Type) (a :: PTypes) where 
  LVId  :: forall a ctx. SingI a => LvIdX ctx a ->  Text    -> LValuable ctx a
  LVIdWithAcc :: forall a b ctx. 
    (SingI a, SingI b) 
    => LvIdWithAccX ctx a b 
    -> Text -> Accessors ctx (a :~> b) -> LValuable ctx b
  -- LvArr :: (SingI a) => LvArrX ctx a -> LValuable ctx (PZArray a) -> E ctx PZ ->  LValuable ctx a
  -- LvDot :: forall a b ctx. 
  --   (SingI a, SingI b) => LvDotX ctx a b -> LValuable ctx a -> Text -> LValuable ctx b

type family LvIdX  (ctx :: Type) (a :: PTypes) :: Type
type family LvIdWithAccX (ctx :: Type) (a :: PTypes) (b :: PTypes) :: Type 

-- type family LvArrX (ctx :: Type) (a :: PTypes) :: Type 
-- type family LvDotX (ctx :: Type) (a :: PTypes) (b :: PTypes) :: Type



data A (ctx :: Type) (actx :: [ActionCtx]) (a :: PTypes) where 
  Assignment :: SingI a => AssX ctx a -> LValuable ctx a -> E ctx a -> A ctx '[] PAUnit 
  Declare    :: SingI a => DeclX ctx a -> LValuable ctx a -> E ctx a -> A ctx '[] PAUnit 
  ForLoop    :: (SingI a,SingI b, SingI actx) 
    => ForX ctx actx a b 
    -> Pattern ctx a 
    -> E ctx a 
    -> A ctx actx b 
    -> A ctx actx b
  WhileLoop :: (SingI a, SingI actx)
    => WhileX ctx actx a
    -> E ctx PZBool 
    -> A ctx actx a
    -> A ctx actx a
  AE :: SingI a 
    => AEX ctx a 
    -> E ctx a 
    -> A ctx '[PureCtx] a
  -- Be wary, we do not enforce that actx == U actx (AECtx)
  Match :: forall a b actx ctx. (SingI a, SingI b, SingI actx)
    => MatchX ctx actx a b 
    -> E ctx a 
    -> NonEmpty (Pattern ctx a, AECtx ctx b)
    -> A ctx actx b
  -- Be wary, we do not enforce that actx = U actX (AEF) U actx(AECtx)
  AS :: forall actx a ctx. (SingI a, SingI actx)
    => ASX ctx a 
    -> AECtx ctx a -- ^ Last instruction
    -> [AEF ctx]   -- ^ Actions in reversed order 
    -> A ctx actx a
  AReturn :: (SingI a)
    => AReturnX ctx a
    -> E ctx a
    -> A ctx '[ReturnCtx] a
  ABreak    :: ABreakX ctx -> A ctx '[LoopCtx] PAUnit 
  AContinue :: AContinueX ctx ->  A ctx '[LoopCtx] PAUnit 
  ABottom   :: forall actx a ctx. ABotX ctx -> A ctx actx a 
  Skip :: forall a ctx. SingI a => SkipX ctx a -> A ctx '[] a

data AECtx (ctx :: Type) (a :: PTypes) where 
  MkAECtx :: forall actx ctx a. SingI actx => A ctx actx a -> AECtx ctx a

data AEF (ctx :: Type) where 
  MkAEF :: forall actx a ctx. (SingI actx, SingI a) => A ctx actx a -> AEF ctx

type family FunDefX  (ctx :: Type) (retT :: PTypes) :: Type 
type family TypeDefX (ctx :: Type) :: Type 
type family AssX       (ctx :: Type) (a :: PTypes) :: Type 
type family DeclX      (ctx :: Type) (a :: PTypes) :: Type 
type family ForX       (ctx :: Type) (actx :: [ActionCtx]) (a :: PTypes) (b :: PTypes) :: Type 
type family WhileX     (ctx :: Type) (actx :: [ActionCtx]) (a :: PTypes) :: Type 
type family AEX        (ctx :: Type) (a :: PTypes) :: Type 
type family MatchX     (ctx :: Type) (actx :: [ActionCtx]) (a :: PTypes) (b :: PTypes)
type family ASX        (ctx :: Type) (a :: PTypes) :: Type
type family AReturnX   (ctx :: Type) (a :: PTypes) :: Type 
type family ABreakX    (ctx :: Type) :: Type 
type family AContinueX (ctx :: Type) :: Type 
type family ABotX      (ctx :: Type) :: Type 
type family SkipX      (ctx :: Type) (a :: PTypes) :: Type

data FunctionArg (ctx :: Type) where 
  MkFunctionArg :: forall a ctx. SingI a 
    => FunArgX ctx a 
    -> Text -> PassOption -> FunctionArg ctx 

type family FunArgX (ctx :: Type) (a :: PTypes) :: Type 

data Definition (ctx :: Type) where 
  FunctionDef :: 
    (SingI retT)
    => FunDefX ctx retT 
    -> Text 
    -> [FunctionArg ctx]
    -> A ctx '[ReturnCtx] retT 
    -> Definition ctx
  TypeDef :: TypeDefX ctx
    -> Text 
    -> Types 
    -> Definition ctx 



-- | Expression AST indexed by a context.
data  E  (ctx :: Type) (a :: PTypes) where
  EZ       :: EZX ctx -> Int   -> E ctx PZ
  EF       :: EFX ctx -> Float -> E ctx PF 
  EZString :: EZStringX ctx -> Text -> E ctx PZString
  EZChar   :: EZCharX ctx -> Char -> E ctx PZChar 
  EZBool   :: EZBoolX ctx -> Bool -> E ctx PZBool
  EPlusZ   :: EPlusZX ctx -> E ctx PZ -> E ctx PZ -> E ctx PZ 
  EPlusZF  :: EPlusZFX ctx -> E ctx PZ -> E ctx PF -> E ctx PF 
  EPlusFZ  :: EPlusFZX ctx -> E ctx PF -> E ctx PZ -> E ctx PF 
  EPlusF   :: EPlusFX ctx -> E ctx PF -> E ctx PF -> E ctx PF

  ETimesZ  :: ETimesZX ctx -> E ctx PZ -> E ctx PZ -> E ctx PZ 
  ETimesF  :: ETimesFX  ctx -> E ctx PF -> E ctx PF -> E ctx PF
  ETimesZF  :: ETimesZFX ctx -> E ctx PZ -> E ctx PF -> E ctx PF 
  ETimesFZ  :: ETimesFZX ctx -> E ctx PF -> E ctx PZ -> E ctx PF 

  EDivF    :: EDivFX  ctx -> E ctx PF -> E ctx PF -> E ctx PF
  EPowerZ  :: EPowerZX ctx -> E ctx PZ -> E ctx PZ -> E ctx PZ 
  EPowerF  :: EPowerFX  ctx -> E ctx PF -> E ctx PF -> E ctx PF
  EModZ    :: EModZX ctx -> E ctx PZ -> E ctx PZ -> E ctx PZ 
  EMinusZ  :: EMinusZX ctx -> E ctx PZ -> E ctx PZ -> E ctx PZ 
  EMinusF  :: EMinusFX  ctx -> E ctx PF -> E ctx PF -> E ctx PF
  EUMinusZ :: EUMinusZX ctx -> E ctx PZ -> E ctx PZ 
  EUMinusF :: EUMinusFX ctx -> E ctx PF -> E ctx PF
  ELTZ     :: ELTZX ctx -> E ctx PZ -> E ctx PZ -> E ctx PZBool  
  ELTF     :: ELTFX  ctx -> E ctx PF -> E ctx PF -> E ctx PZBool 
  EGTZ     :: EGTZX ctx -> E ctx PZ -> E ctx PZ -> E ctx PZBool 
  EGTF     :: EGTFX  ctx -> E ctx PF -> E ctx PF -> E ctx PZBool 
  ENEQZ    :: ENEQZX ctx -> E ctx PZ -> E ctx PZ -> E ctx PZBool 
  ENEQF    :: ENEQFX  ctx -> E ctx PF -> E ctx PF -> E ctx PZBool 
  EEQZ     :: EEQZX ctx -> E ctx PZ -> E ctx PZ -> E ctx PZBool 
  EEQF     :: EEQFX  ctx -> E ctx PF -> E ctx PF -> E ctx PZBool 
  EGTEQTZ  :: EGTEQTZX ctx -> E ctx PZ -> E ctx PZ -> E ctx PZBool 
  EGTEQTF  :: EGTEQTFX  ctx -> E ctx PF -> E ctx PF -> E ctx PZBool 
  EELEQTZ  :: EELEQTZX ctx -> E ctx PZ -> E ctx PZ -> E ctx PZBool 
  EELEQTF  :: EELEQTFX  ctx -> E ctx PF -> E ctx PF -> E ctx PZBool 
  EAnd     :: EAndX ctx -> E ctx PZBool -> E ctx PZBool -> E ctx PZBool 
  EOr      :: EOrX ctx -> E ctx PZBool -> E ctx PZBool -> E ctx PZBool 
  ENeg     :: ENegX ctx -> E ctx PZBool -> E ctx PZBool 
  EABlock  :: forall a actx ctx. 
    (SingI a, Elem PureCtx actx ~ True, NotElem ReturnCtx actx ~ True) 
    => EABlockX ctx a -> A ctx actx a -> E ctx a 
  EBottom :: EBotX ctx -> E ctx PBottom 
  EError  :: forall a ctx. EErrorX ctx a -> E ctx a
  EVar :: forall a ctx. SingI a => EVarX ctx a -> Text -> E ctx a 
  EApp :: forall a b ctx. (SingI a,SingI b) 
    => EAppX ctx a b -> E ctx (a :~> b) -> E ctx a -> E ctx b

type family EAppX (ctx :: Type) (a :: PTypes) (b :: PTypes) :: Type
type family EVarX (ctx :: Type) (a :: PTypes) :: Type
type family EPlusZX   (ctx :: Type) :: Type 
type family EPlusFX   (ctx :: Type) :: Type 
type family EZX       (ctx :: Type) :: Type 
type family EFX       (ctx :: Type) :: Type 
type family EZStringX  (ctx :: Type) :: Type
type family EZCharX    (ctx :: Type) :: Type 
type family ETimesZX  (ctx :: Type) :: Type 
type family ETimesFX  (ctx :: Type) :: Type 
type family EDivFX    (ctx :: Type) :: Type 
type family EPowerZX  (ctx :: Type) :: Type 
type family EPowerFX  (ctx :: Type) :: Type 
type family EModZX    (ctx :: Type) :: Type 
type family EMinusZX  (ctx :: Type) :: Type 
type family EMinusFX  (ctx :: Type) :: Type 
type family EUMinusZX (ctx :: Type) :: Type 
type family EUMinusFX (ctx :: Type) :: Type 
type family ELTZX     (ctx :: Type) :: Type 
type family ELTFX     (ctx :: Type) :: Type 
type family EGTZX     (ctx :: Type) :: Type 
type family EGTFX     (ctx :: Type) :: Type 
type family ENEQZX    (ctx :: Type) :: Type 
type family ENEQFX    (ctx :: Type) :: Type 
type family EEQZX     (ctx :: Type) :: Type 
type family EEQFX     (ctx :: Type) :: Type 
type family EELEQTZX  (ctx :: Type) :: Type 
type family EELEQTFX  (ctx :: Type) :: Type 
type family EGTEQTZX  (ctx :: Type) :: Type 
type family EGTEQTFX  (ctx :: Type) :: Type 
type family EAndX     (ctx :: Type) :: Type 
type family EOrX      (ctx :: Type) :: Type 
type family ENegX     (ctx :: Type) :: Type 
type family EABlockX  (ctx :: Type) (a :: PTypes) :: Type 
type family EBotX     (ctx :: Type) :: Type 
type family EErrorX   (ctx :: Type) (a :: PTypes) :: Type
type family EPlusZFX  (ctx :: Type) :: Type 
type family EPlusFZX  (ctx :: Type) :: Type 
type family EZBoolX   (ctx :: Type) :: Type
type family ETimesZFX  (ctx :: Type) :: Type 
type family ETimesFZX  (ctx :: Type) :: Type 



instance Show (Pattern ctx a) where 
  show (PaId _ var opt) = case opt of 
    POValue -> T.unpack var 
    PORef   -> T.unpack var <> " by reference"
  show _ = undefined 

instance Show (LValuable ctx a) where 
  show _ = undefined 

instance Show (Accessors ctx a) where 
  show _ = undefined 
instance Show (A ctx actx a) where 


-- https://stackoverflow.com/questions/27471937/showsprec-and-operator-precedences
instance Show (E ctx a) where
  showsPrec p = \case 
    -- Infix Left with precedence 4
    EPlusZ _ a b -> showParen (p > 4) $ showsPrec 4 a . showString " + " . showsPrec 5 b
    _ -> undefined
