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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Prelude.Singletons hiding (Map) 
import Data.List.NonEmpty 
import Data.Kind (Type)
import Data.Singletons.Decide
import Data.Map (Map)
import Data.Map qualified as M
import Data.Foldable (foldrM)
import Data.Functor.Identity

data TCCtx

data FIdentity (x :: PTypes) = FIdentity Types 
type TypeDict = TypeRepMap PTypes FIdentity
type ErrLogs = [Text]

data TypeCheckEnv = TCE 
  { tceDict :: TypeDict 
  , tceExpectedType :: Maybe Types
  , tceTypesDict :: Map Text Types  
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




typeCheckTypes :: forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) 
  => L.PTypes AlexPosn -> m Types
typeCheckTypes (L.PAtom a _) = pure . ZAtom $ TCon a []
typeCheckTypes (L.PId a pos) = ask >>= \(TCE{tceTypesDict=te}) -> case M.member a te of 
  True -> pure . ZId $ a
  False -> do 
    appendToLog 
      $ "Scoping Error. Type "  
      <> T.show a
      <> ", At position: " 
      <> T.show pos 
      <> " not defined. Defaulting to Bottom."
    pure Bottom
typeCheckTypes (L.PBool _)      = pure ZBool 
typeCheckTypes (L.PInt _)       = pure Z 
typeCheckTypes (L.PFloat _)     = pure F
typeCheckTypes (L.PChar _)      = pure ZChar 
typeCheckTypes (L.PString _)    = pure ZString 
typeCheckTypes (L.PUnit _)      = pure ZUnit 
typeCheckTypes (L.PVoid _)      = pure ZVoid 
typeCheckTypes (L.PVector t _)  = ZArray <$> typeCheckTypes t
typeCheckTypes (L.PUnion a b )  = (:|:) <$> typeCheckTypes a <*> typeCheckTypes b
typeCheckTypes (L.PRecord (L.RecordPattern vtp  vtps _)) = Record <$> do 
  let vts = (\(a,b,_) -> (a,b)) <$> vtp : vtps
  let f (v,t) = (v :~.:) <$> typeCheckTypes t
  traverse f vts


 

typeCheckFunArg :: forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) 
  => L.FunArg AlexPosn -> m (FunctionArg TCCtx)
typeCheckFunArg (L.FunArg t var mode pos) = do 
  t' <- typeCheckTypes t
  case toSing t' of 
    SomeSing @_ @st st 
      -> withSingI st 
      $ pure 
      $ MkFunctionArg @st @TCCtx pos var 
      $ maybe POValue (const PORef) mode

generateAccessorFunctions :: MonadReader Types m =>  Types -> m (Map Text Types )
generateAccessorFunctions (a :|: b) 
  = liftA2 M.union (generateAccessorFunctions a) (generateAccessorFunctions b)
generateAccessorFunctions (a :~.: b) = ask >>= \t -> pure 
  $ M.singleton (getAccessorName a t) (t :-> b)
generateAccessorFunctions (Record ts) = M.unions <$> traverse generateAccessorFunctions ts
generateAccessorFunctions _ = pure $ M.empty


getAccessorName :: Text -> Types -> Text 
getAccessorName f t = f <> "<" <> T.show t <> ">"

generateFunType :: forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) 
  => Types -> L.FunArgs AlexPosn -> m Types
generateFunType ret = foldrM (\(L.FunArg la _ _ _) b -> (:-> b) <$> typeCheckTypes la ) ret

typeCheckDef :: forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) 
  => L.Definition AlexPosn -> m (TypeCheckEnv,Definition TCCtx)
typeCheckDef (L.TypeDef tname at pos) = do 
  env <- ask 
  -- local allows recursive types.
  t <- local (\env' -> env'{tceTypesDict = M.insert tname (ZId tname) $ tceTypesDict env'}) 
    $ typeCheckTypes at
  let env0 = env{tceTypesDict = M.insert tname t $ tceTypesDict env}
  pure (env0, TypeDef pos tname t)
typeCheckDef (L.FunctionDef reT fname args as pos) = do
  env     <- ask
  reT'    <- typeCheckTypes reT 
  funType <- generateFunType reT' args 
  funArgs <- traverse typeCheckFunArg args 
  SomeSing @_ @sFunType sFunType <- pure $ toSing funType 
  SomeSing @_ @sRet sRet <- pure $ toSing reT'
  withSingI sFunType 
    $ withSingI sRet 
    $ do 
      -- On error return the bottom of actions 
      let iError = withSingI sRet $ FunctionDef @sRet pos fname funArgs $ ABottom @'[ReturnCtx] @sRet pos 
        -- Allows recursive functions 
      withDeclared @sFunType fname  (pure $ (env,iError)) $ do 
        env' <- ask 
        MkAEF @actx @aT as'   
          <- fmap snd 
          $ withSingI sRet 
          $ withSingI sFunType 
          $ withExpectedType (Just reT') 
          $ typeCheckActions as 
        case (sing @actx) of 
          SCons SReturnCtx SNil 
            -> pure 
              ( env'
              , FunctionDef @aT pos fname funArgs as' 
              )
          _ -> do 
            appendToLog 
              $ "Syntax Error: The function" <> fname <>  " at "
              <> T.show pos <> " needs an explicit return."
            pure (env', iError)
  

type instance FunArgX TCCtx a = AlexPosn 
type instance TypeDefX TCCtx = AlexPosn 
type instance FunDefX TCCtx a = AlexPosn
type instance ABotX TCCtx = AlexPosn 

typeCheckAction ::  forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) => L.Action AlexPosn -> m (TypeCheckEnv, AEF TCCtx)
typeCheckAction _ = undefined 

typeCheckActions :: forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) => [L.Action AlexPosn] -> m (TypeCheckEnv, AEF TCCtx)
typeCheckActions = undefined 

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
  

withDeclared :: forall (x :: PTypes)  m a.
  ( MonadReader TypeCheckEnv m
  , MonadWriter ErrLogs m
  , MonadIO m
  , SingI x
  ) => Text -> m a -> m a -> m a
withDeclared v iError ma =do 
  eEnv <- tceDict <$> ask 
  nEnv <- declare @x v eEnv 
  case nEnv of 
    Left e -> appendToLog (showGammaErrors e) >> iError 
    Right nEnv' -> local (\env' -> env'{tceDict=nEnv'}) ma 


