{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
module TypeCheck where 

import AST 
import Types 
import Parser.LexerDefinitions qualified as L
import Parser.Lexer 
import Control.Monad.Reader 
import Control.Monad.Writer 
import TypedMap 
import Data.Text (Text)
import Data.Text qualified as T 
import Control.Monad 
import Prelude.Singletons 
import Data.List.NonEmpty 
import Data.Kind (Type)
import Data.Singletons.Decide

data TCCtx
type TypeDict = TypeRepMap PTypes (E TCCtx)
type ErrLogs = [Text]

data TypeCheckEnv = TCE 
  { tceDict :: TypeDict 
  , tceExpectedType :: Maybe Types
  }

appendToLog :: MonadWriter ErrLogs m => Text -> m ()
appendToLog = tell . pure 

showGammaErrors :: GammaErrors -> Text 
showGammaErrors e = case e of 
  TypeMismatch s (ExpectedType et) (ActualType at) 
    -> "Type Mismatch at symbol: " 
    <> s 
    <> ". Expected type: "
    <> et 
    <> "but instead got: "
    <> at
  VariableNotDefined s 
    -> "Symbol not defined: " <> s
  VariableAlreadyDeclared s 
    -> "Symbol cannot be re-declared: " <> s
  VariableNotInitialized s 
    -> "Symbol not initialized: " <> s

data PatternE where 
  MkPatternE :: SingI a => Pattern TCCtx a -> PatternE

type instance PaBX TCCtx = AlexPosn 

typeCheckPattern :: forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) 
  => L.Pattern AlexPosn -> m (TypeCheckEnv, PatternE)
typeCheckPattern (L.PaBool "True" p) = ask >>= \env -> pure (env,MkPatternE $ PaB p True)
typeCheckPattern _ = undefined


data LValuableE where 
  MkLValuableE :: SingI a => LValuable TCCtx a -> LValuableE

data EE where 
  MkEE :: SingI a => E TCCtx a -> EE 

typeCheckExpression :: 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  )
  => L.Expression AlexPosn 
  -> m EE 
typeCheckExpression le@(L.Plus l r pos) = do 
  MkEE @l' l' <- withExpectedType Nothing $ typeCheckExpression l
  MkEE @r' r' <- withExpectedType Nothing $ typeCheckExpression r 
  case (sing @l', sing @r') of 
    ( STCon (matches @"Z" -> Just Refl) SNil
      , STCon (matches @"Z" -> Just Refl) SNil 
      ) -> pure . MkEE $ EPlusZ pos l' r'
    ( STCon (matches @"F" -> Just Refl) SNil
      , STCon (matches @"F" -> Just Refl) SNil 
      ) -> pure . MkEE $ EPlusF pos l' r'
    ( STCon (matches @"Z" -> Just Refl) SNil
      , STCon (matches @"F" -> Just Refl) SNil 
      ) -> error "TODO"
    ( STCon (matches @"F" -> Just Refl) SNil
      , STCon (matches @"Z" -> Just Refl) SNil 
      ) -> error "TODO"
    (STCon (matches @"Bottom" -> Just Refl) _, _) 
      -> pure $ MkEE l'
    (_,STCon (matches @"Bottom" -> Just Refl) _) 
      -> pure $ MkEE r'

    _ -> do 
      appendToLog 
        $ "Type Error. Incoercible types encountered at the sum: "  
        <> T.show le 
        <> ". At position: " 
        <> T.show pos 
        <> ". Expected Type: Two numeric types"
        <> ", but instead got: ("
        <> T.show l 
        <> ") : "
        <> T.show (demote @l')
        <> ", ("
        <> T.show r
        <> ") : "
        <> T.show (demote @r')
        <> "."
      pure . MkEE $ EBottom pos
typeCheckExpression _ = undefined


type instance EPlusZX TCCtx = AlexPosn 
type instance EPlusFX TCCtx = AlexPosn 
type instance EBotX   TCCtx = AlexPosn 

withExpectedType :: (MonadReader TypeCheckEnv m) 
  => Maybe Types -> m a -> m a
withExpectedType t = local (\e -> e{tceExpectedType = t})
  
