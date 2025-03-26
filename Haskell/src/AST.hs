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
module AST where 


import Data.Kind (Type, Constraint)
import Types
import Data.Text (Text)
import Data.List.NonEmpty
import Prelude.Singletons
import Data.Singletons.TH 
import Data.List.Singletons 

$(singletons [d|
  data ActionCtx 
    = LoopCtx 
    | ReturnCtx
    | PureCtx

  data PassOption 
    = POValue 
    | PORef

  |])

data Pattern (ctx :: Type) (a :: PTypes) where 
  PaZ      :: PaZX ctx   -> Int    -> Pattern ctx PZ 
  PaF      :: PaFX ctx   -> Float  -> Pattern ctx PF 
  PaB      :: PaBX ctx   -> Bool   -> Pattern ctx PZBool 
  PaChar   :: PaCharX ctx -> Char   -> Pattern ctx PZChar 
  PaString :: PaStringX ctx -> Text -> Pattern ctx PZString 
  PaId     :: SingI a => PaIdX ctx a -> Text   -> PassOption -> Pattern ctx a 
  PaRecord :: forall a ctx. 
    (SingI a, IsRecord a ~ True)
    => Pattern ctx a -> Demote (KindOf a) -> Pattern ctx a

type family PaZX      (ctx :: Type) :: Type 
type family PaFX      (ctx :: Type) :: Type 
type family PaBX      (ctx :: Type) :: Type 
type family PaCharX   (ctx :: Type) :: Type 
type family PaStringX (ctx :: Type) :: Type 
type family PaIdX     (ctx :: Type) (a :: PTypes) :: Type 


data LValuable (ctx :: Type) (a :: PTypes) where 
  LVId  :: SingI a => LvIdX ctx a ->  Text    -> LValuable ctx a
  LvArr :: (SingI a, SingI b) => LvArrX ctx a b -> LValuable ctx a -> E ctx PZ ->  LValuable ctx b
  LvDot :: (SingI a, SingI b) => LvDotX ctx a b -> LValuable ctx a -> Text -> LValuable ctx b

type family LvIdX  (ctx :: Type) (a :: PTypes) :: Type 
type family LvArrX (ctx :: Type) (a :: PTypes) (b :: PTypes) :: Type 
type family LvDotX (ctx :: Type) (a :: PTypes) (b :: PTypes) :: Type



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
  AS :: (SingI a, SingI actx)
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


data AECtx (ctx :: Type) (a :: PTypes) where 
  MkAECtx :: forall actx ctx a. SingI actx => A ctx actx a -> AECtx ctx a

data AEF (ctx :: Type) where 
  MkAEF :: forall actx a ctx. (SingI actx, SingI a) => A ctx actx a -> AEF ctx


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

-- | Expression AST indexed by a context.
data  E  (ctx :: Type) (a :: PTypes) where
  EZ       :: EZX ctx -> Int   -> E ctx PZ
  EF       :: EFX ctx -> Float -> E ctx PF 
  EZString :: EZString ctx -> Text -> E ctx PZString
  EZChar   :: EZChar ctx -> Char -> E ctx PZChar 
  EPlusZ   :: EPlusZX ctx -> E ctx PZ -> E ctx PZ -> E ctx PZ 
  EPlusF   :: EPlusFX ctx -> E ctx PF -> E ctx PF -> E ctx PF
  ETimesZ  :: ETimesZX ctx -> E ctx PZ -> E ctx PZ -> E ctx PZ 
  ETimesF  :: ETimesFX  ctx -> E ctx PF -> E ctx PF -> E ctx PF
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


type family EPlusZX   (ctx :: Type) :: Type 
type family EPlusFX   (ctx :: Type) :: Type 
type family EZX       (ctx :: Type) :: Type 
type family EFX       (ctx :: Type) :: Type 
type family EZString  (ctx :: Type) :: Type
type family EZChar    (ctx :: Type) :: Type 
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


