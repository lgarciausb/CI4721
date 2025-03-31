
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
module TypedMap where


import Data.Map (Map)
import qualified Data.Map as M

import Unsafe.Coerce
import Type.Reflection 
--import Control.Lens.Lens
import Data.Proof

import Debug.Trace
import Data.List.NonEmpty ( NonEmpty((:|)) )
import Data.Foldable1 (foldl1')
import Control.Concurrent hiding (yield)
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative hiding (yield)
import Data.Singletons
import Data.String (IsString(..))
import Data.Singletons.Decide 
import Control.Monad.Reader
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Functor 

type Symbol = Text 

-------------------------------
-- Errors
--------------------------------

data ExpectedType = ExpectedType Text  
data ActualType = ActualType Text

data GammaErrors
  =  TypeMismatch Symbol ExpectedType ActualType 
  |  VariableNotDefined Symbol
  |  VariableAlreadyDeclared Symbol
  |  VariableNotInitialized Symbol

--------------------------------
-- Lens interface
--------------------------------

type family EvalMonad (f :: a -> Type) :: Type -> Type

-- Defines a way to get, set, set fresh and obtain the name of a variable
data LensM k (f :: k -> Type) (a :: k) = LensM
  { getL  ::  TypeRepMap k f -> (EvalMonad f) (Either GammaErrors (f a))
  , setL  ::  TypeRepMap k f -> f a -> (EvalMonad f) (Either GammaErrors (TypeRepMap k f))
  , declareL ::  TypeRepMap k f -> (EvalMonad f) (Either GammaErrors (TypeRepMap k f))
  , declareFL ::  TypeRepMap k f -> (EvalMonad f) (TypeRepMap k f)
  , varNameM :: Symbol
  }

viewM ::LensM k f a -> TypeRepMap k f -> (EvalMonad f)(Either GammaErrors (f a))
viewM  = getL

setM :: LensM k f a -> f a -> TypeRepMap k f -> (EvalMonad f) (Either GammaErrors (TypeRepMap k f))
setM = flip . setL

declareM :: LensM k f a ->  TypeRepMap k f -> (EvalMonad f) (Either GammaErrors (TypeRepMap k f))
declareM = declareL

declareMF :: LensM k f a ->  TypeRepMap k f -> (EvalMonad f) (TypeRepMap k f)
declareMF = declareFL


data Any k (f :: k -> Type)   where
  MkAny :: forall {k} (a :: k) f.  (SingI a, SingKind k, Show (Demote k)) => MVar (f a) -> Any k f 

data Any' k (f :: k -> Type)   where
  MkAny' :: forall {k} (a :: k) f.  (SingI a, SingKind k, Show (Demote k)) => f a -> Any' k f 


newtype TypeRepMap k f = TypeRepMap (Map Symbol (Any k f))

empty :: TypeRepMap k f
empty = TypeRepMap M.empty 

scope :: TypeRepMap k f -> [Symbol]
scope (TypeRepMap m) = M.keys m 

inScope :: Symbol -> TypeRepMap k f -> Bool 
inScope s (TypeRepMap m) =  s `M.member` m 


notInScope :: Symbol -> TypeRepMap k f -> Bool 
notInScope s (TypeRepMap m) = not $ s `M.member` m 


declareFresh :: forall {k} (a :: k) (f :: k -> Type) m.  
  ( SingI a
  , MonadIO m
  , Show (Demote k)
  , SingKind k
  ) => Symbol -> TypeRepMap k f -> m (TypeRepMap k f)
declareFresh var (TypeRepMap d) = do 
  mv <- liftIO $ newEmptyMVar @(f a)
  pure . TypeRepMap $ M.insert var (MkAny mv) d

declare :: forall {k} (a :: k) (f :: k -> Type) m.  
  ( SingI a
  , MonadIO m
  , Show (Demote k)
  , SingKind k
  ) => Symbol -> TypeRepMap k f -> m (Either GammaErrors (TypeRepMap k f))
declare var (TypeRepMap d) = case M.lookup var d of 
  Just _ -> pure . Left $ VariableAlreadyDeclared var 
  Nothing -> do 
    mv <- liftIO $ newEmptyMVar @(f a)
    pure . pure . TypeRepMap $ M.insert var (MkAny mv) d

insert :: forall {k} (a :: k) (f :: k -> Type) m.  
  ( SingI a
  , MonadIO m
  , SDecide k
  , Show (Demote k)
  , SingKind k
  ) => Symbol -> f a -> TypeRepMap k f -> m (Either GammaErrors (TypeRepMap k f))
insert var e (TypeRepMap d) = case M.lookup var d of 
  Nothing -> do 
    mv <- liftIO $ newMVar e
    pure . pure . TypeRepMap $ M.insert var (MkAny mv) d
  Just (MkAny  @a' mv) -> case decideEquality (sing @a') (sing @a) of
    Nothing -> pure . Left $ TypeMismatch var (ExpectedType . T.show $ demote @a') (ActualType . T.show $ demote @a )
    Just Refl -> do 
      liftIO (modifyMVar_ mv $ pure . (const e) ) 
      pure (Right . TypeRepMap $ d) 

insertFresh  :: forall {k} (a :: k) (f :: k -> Type) m.  
  ( SingI a
  , MonadIO m
  , SDecide k
  , Show (Demote k)
  , SingKind k
  ) => Symbol -> f a -> TypeRepMap k f -> m (TypeRepMap k f)
insertFresh var e (TypeRepMap d) = do 
  mv <- liftIO $ newMVar e
  pure . TypeRepMap $ M.insert var (MkAny mv) d

  -- = declareFresh @a var (TypeRepMap d) 
  -- >>= insert var e
  -- >>= (\(Right a) -> pure a)




yield ::  forall {k} (a :: k) (f :: k -> Type) m.  
  ( SingI a
  , MonadIO m
  , SDecide k
  , Show (Demote k)
  , SingKind k
  ) => Symbol -> TypeRepMap k f -> m (Either GammaErrors (f a))
yield var (TypeRepMap d) = case M.lookup var d of 
  Nothing -> pure . Left $ VariableNotDefined var
  Just (MkAny @a' mv) -> case decideEquality (sing @a') (sing @a) of
    Nothing   -> pure . Left $ TypeMismatch var (ExpectedType . T.show $ demote @a') (ActualType . T.show $ demote @a )
    Just Refl -> liftIO (tryReadMVar mv) >>= \case 
      Nothing -> pure . Left $ VariableNotInitialized var 
      Just c  -> pure . pure $ c

yield' ::  forall {k}  (f :: k -> Type) m.  
  ( MonadIO m
  , SingKind k
  ) => Symbol -> TypeRepMap k f -> m (Either GammaErrors (Any' k f))
yield' var (TypeRepMap d) = case M.lookup var d of 
  Nothing -> pure . Left $ VariableNotDefined var
  Just (MkAny  mv) -> Right . MkAny' <$> liftIO (readMVar mv)



instance forall k  (a :: k) (f :: k -> Type). 
  ( SingI a
  , SDecide k
  , Show (Demote k)
  , SingKind k
  , MonadIO (EvalMonad f)
  ) => IsString (LensM k f a) where
  fromString (fromString -> var) = LensM 
    (yield @a var) 
    (flip $ insert var) 
    (declare @a var) 
    (declareFresh @a var)
    var


mkVar :: forall k  (a :: k) (f :: k -> Type). 
  ( SingI a
  , SDecide k
  , Show (Demote k)
  , SingKind k
  , MonadIO (EvalMonad f)
  ) => String -> LensM k f a
mkVar = fromString

