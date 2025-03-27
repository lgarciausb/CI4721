
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE TypeAbstractions         #-} 
{-# Language PatternSynonyms          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ImportQualifiedPost      #-}
{-# LANGUAGE TemplateHaskell          #-}
-- Template haskell warnings
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE ViewPatterns #-}

module Types where



import Data.Singletons.TH (singletons,genSingletons, promote)
import Prelude.Singletons hiding (Const)
    -- ( (%&&),
    --   (%<$>),
    --   LiftA2Sym0,
    --   PMonad(type (>>=)),
    --   SApplicative(sLiftA2),
    --   type (&&@#@$),
    --   type (==@#@$),
    --   PEq(type (==)),
    --   SEq((%==)),
    --   type (<$>@#@$),
    --   FalseSym0,
    --   JustSym0,
    --   NothingSym0,
    --   SBool(SFalse, STrue),
    --   SMaybe(SJust, SNothing),
    --   TrueSym0)

import Data.Singletons.Decide
import Data.Kind (Constraint, Type)
import Data.Proof
import Control.Applicative (Const(..))
import Debug.Trace (trace)
import Data.String 
import Data.String.Singletons
import Data.List.Singletons
import GHC.TypeLits.Singletons
import GHC.TypeLits
import Data.Singletons.TH.Options
import Language.Haskell.TH qualified as TH 
import Data.Text (Text)
import Data.Text qualified as Text
import Unsafe.Coerce
import Data.List (intercalate)


type Name  = Text
type PName = Symbol 



data Types = TCon Name [Types]
data PTypes = PTCon PName  [PTypes]


$(let customPromote :: TH.Name -> TH.Name 
      customPromote n 
        | n == ''Types  = ''PTypes
        | n == 'TCon    = 'PTCon 
        | n == ''Text   = ''Symbol
        | n == ''Name   = ''Symbol
        | otherwise = promotedDataTypeOrConName defaultOptions n

      customDefun :: TH.Name -> Int -> TH.Name 
      customDefun n sat = defunctionalizedName defaultOptions (customPromote n) sat in
  withOptions defaultOptions{ promotedDataTypeOrConName = customPromote
                            , defunctionalizedName      = customDefun
                            } $ do
    ds0 <- genSingletons [''Types] 
    ds1 <- singletons [d|
      instance Eq Types where 
        TCon x as == TCon y bs = (x == y) && (as == bs)
      |]
    pure $ ds0 <> ds1 
  )


infixr 0 :->
infixr 2 :|:
infix  1 :~.:

pattern Z         = TCon "Z"      []
pattern F         = TCon "F"      []
pattern ZString   = TCon "String" []
pattern ZBool     = TCon "Bool"   []
pattern ZChar     = TCon "Char"   []
pattern ZUnit     = TCon "Unit"   []
pattern ZVoid     = TCon "Void"   []
pattern ZAtom  a  = TCon "Atom"   [a]
pattern a :-> b   = TCon "->"     [a, b]
pattern a :|: b   = TCon "|"      [a,b]
pattern ZArray a  = TCon "Array" [a]
pattern ZRef a    = TCon "Ref" [a]
pattern Record as = TCon "Record" as 
pattern a :~.: b  = TCon "~." [a,b]
pattern TString a = TCon a []
pattern AUnit     = TCon "AUnit" []
pattern Bottom    = TCon "Bottom" []



infixr 0 :~>
infixr 2 :~|:
infix  1 :~~.:

type PZ         = PTCon "Z"      '[]
type PF         = PTCon "F"      '[]
type PZString   = PTCon "String" '[]
type PZBool     = PTCon "Bool"   '[]
type PZChar     = PTCon "Char"   '[]
type PZUnit     = PTCon "Unit"   '[]
type PZVoid     = PTCon "Void"   '[]
type PZAtom  a  = PTCon "Atom"   '[a]
type a :~> b    = PTCon "->"     '[a, b]
type a :~|: b   = PTCon "|"      '[a,b]
type PZArray a  = PTCon "Array" '[a]
type PZRef a    = PTCon "Ref"   '[a]
type PRecord as = PTCon "Record" as
type a :~~.: b  = PTCon "~." [a,b]
type PTString a = PTCon a '[]
type PAUnit     = PTCon "AUnit" '[]
type PBottom    = PTCon "Bottom" '[]

type family IsRecord (a :: PTypes) :: Bool where 
  IsRecord (PTCon "Record" _) = True 
  IsRecord _                  = False

matches :: forall (a :: Symbol) (b :: Symbol). SingI a => SSymbol b -> Maybe (b :~: a)
matches b@(SSymbol ) =  case sing @a of
  SSymbol -> sameSymbol b (SSymbol @a)


instance Show Types where 
  showsPrec p = \case 
    TCon a [] -> showString $ Text.unpack a
    a :~.: b -> shows a . showString " : " . shows b
    Record as -> showString "{" . showString (intercalate "," $ show <$> as) . showString "}" 
    a :|: b -> showParen (p > 2) $ showsPrec 3 a . showString " | " . showsPrec 2 b
    a :-> b -> showParen (p > 0) $ showsPrec 1 a . showString " -> " . showsPrec 0 b
    TCon a (x:xs) 
      -> showString (Text.unpack a) . showString "<" 
      . (foldr (\arg acc -> shows arg . showString ", " . acc) (shows x) xs) 
      . showString ">"



instance SDecide PTypes where
  STCon a as %~ STCon b bs = case (a %~ b, as %~ bs) of 
    (Proved Refl, Proved Refl) -> Proved Refl
    (Disproved f,_) -> Disproved (\Refl -> f Refl)
    (_,Disproved f) -> Disproved (\Refl -> f Refl)


--------------------------
-- Utility functions
--------------------------

-- | Implicit equality.
decideEquality' :: forall {k} (a :: k) (b :: k).  (SDecide k, SingI a, SingI b) => Maybe (a :~: b) 
decideEquality' = decideEquality (sing @a) (sing @b)

--------------------------
-- Dictionary Injection
--------------------------

-- | Show implicit singleton.
withShow :: forall (z :: PTypes). SingI z => String
withShow = show $ fromSing (sing @z) 

