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
import Data.Foldable (foldrM,foldlM)
import Data.Functor.Identity

data TCCtx

type TypeDict = TypeRepMap PTypes Sing
type ErrLogs = [Text]

data TypeCheckEnv = TCE 
  { tceDict :: TypeDict 
  , tceExpectedType :: Maybe Types
  , tceTypesDict :: Map Text Types  
  , tceAccessors :: Map Text [Types]
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

genAccessors :: MonadReader Types m =>  Types -> m (Map Text [Types])
genAccessors (a :|: b) 
  =  (M.unionsWith mappend) <$> traverse genAccessors [a, b]
genAccessors (a :~.: b) = ask >>= \t -> pure $ M.singleton a [t :-> b]
genAccessors (Record ts) = M.unionsWith mappend <$> traverse genAccessors ts
genAccessors _ = pure $ M.empty


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
  -- update the types 
  let env0 = env{tceTypesDict = M.insert tname t $ tceTypesDict env}
  -- update the accessor functions 
  env1 <- (\b ta f -> foldrM f b ta) env0 (M.toList $ generateAccessorFunctions t `runReader` (ZId tname)) $ \(n,t') env' -> do
    SomeSing  st <- pure $ toSing t' 
    md <- withSingI st $ TypedMap.insert n st $ tceDict env' 
    case md of 
      Right dict' -> pure env{tceDict = dict'}
      Left _ -> error "impossible case"
  -- update the accessors. 
  let env2 = env1{tceAccessors = M.unionsWith mappend [tceAccessors env1, genAccessors t `runReader` ZId tname] }
  pure (env2, TypeDef pos tname t)
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
typeCheckAction (L.Declare lt var (Just le)) = do 
  env <- ask 
  t   <- typeCheckTypes lt 
  let pos = L.getPTypesInfo lt 
  SomeSing @_ @st st <- pure $ toSing t 
  -- No local means no value recursion.
  MkEE @et e <- withExpectedType (Just t) $ typeCheckExpression le
  -- when (var `inScope` tceDict env) $ appendToLog 
  --   $ "Variable " <> var 
  --   <> " at position: " <> T.show (L.getPTypesInfo lt) 
  --   <> " already declared"
  withSingI st 
    $ withDeclared @st var (pure (env,MkAEF $ ABottom @'[] @PAUnit pos))
    $ ask >>= \env' -> case decideEquality' @et @st of 
      Just Refl -> pure (env', MkAEF $ Declare pos (LVId @st pos var) e ) 
      Nothing -> do 
        appendToLog $ 
          "Type mismatch on the initialization of the variable: "
          <> var 
          <> " at "
          <> T.show pos 
          <> " . Declared type: "
          <> T.show t 
          <> ", but instead got an expression of type"
          <> T.show (demote @et)
          <> "."
        pure (env', MkAEF $ ABottom @'[] @PAUnit pos)
typeCheckAction (L.Assign lv e) = undefined 
typeCheckAction _ = undefined 

type instance DeclX TCCtx a = AlexPosn 
type instance LvIdX TCCtx a = AlexPosn 

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

typeCheckLValue :: forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) 
  => L.LValuable AlexPosn -> m LValuableE
typeCheckLValue (L.PLId var pos) = do 
  env <- ask 
  mV  <- yield' var $ tceDict env 
  case mV of 
    Left e -> do 
      appendToLog $ "Scoping error at " <> T.show pos <> ": " <> showGammaErrors e 
      pure  $ (MkLValuableE $ LVId @PBottom pos var)
    Right (MkAny' @t _) -> pure $ (MkLValuableE $ LVId @t pos var)
typeCheckLValue (L.PLDot var lv pos) = do
  MkLValuableE @t _ <- typeCheckLValue $ L.PLId var pos 

  undefined 

data AccessorsE where 
  MkAccessorsE :: SingI a => Accessors TCCtx a -> AccessorsE 

typeCheckAccessors :: forall (t :: PTypes) m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  , SingI t -- type of the parent. i.e: a.b => t ~ a
  ) =>  L.LValuable AlexPosn -> m AccessorsE 
typeCheckAccessors (L.PLId var pos) = do 
  env <- ask 
  -- ask if var is in the accessor space 
  let mv0  
        = concatMap (\(a :-> b) -> if a == demote @t then [b] else []) 
        <$> M.lookup var (tceAccessors env)
  case mv0 of 
    Nothing -> do 
      appendToLog $ "Scoping error at" <> T.show pos <> ", Accessor " <> var <> " not found"
      pure (MkAccessorsE $ AccSimple @t @PBottom pos var)
    Just [] -> do 
      appendToLog 
        $ "Type error at" 
        <> T.show pos 
        <> ", No Accessors named " 
        <> var <> " were found for type: "
        <> T.show (demote @t)
        <> "."
      pure (MkAccessorsE $ AccSimple @t @PBottom pos var)
    Just (_:_:_) -> do 
      appendToLog 
        $ "Scoping error at" 
        <> T.show pos 
        <> ", Multiple Accessors named " 
        <> var <> " were found for type: "
        <> T.show (demote @t)
        <> "."
      pure (MkAccessorsE $ AccSimple @t @PBottom pos var)
    Just [x] -> do 
      SomeSing @_ @sb sb <- pure $ toSing x 
      withSingI sb $ pure $ MkAccessorsE $ AccSimple @t @sb pos var 
typeCheckAccessors (L.PLDot var lv pos) = do 
  MkAccessorsE @sh h <- typeCheckAccessors @t (L.PLId var pos)
  case sing @sh of 
    STCon (matches @"->" -> Just Refl) (SCons sa (SCons @_ @sb sb SNil)) -> withSingI sa $ withSingI sb $ do 
      MkAccessorsE @st t <- typeCheckAccessors @sb lv 
      case sing @st of 
        STCon (matches @"->" -> Just Refl) (SCons sb' (SCons  sc SNil)) -> withSingI sb' $ withSingI sc $ do 
          case decideEquality sb sb' of 
            Just Refl -> pure $ MkAccessorsE $ AccDot pos h t
            Nothing -> do 
              appendToLog $ "Type error at " <> T.show pos <> ". Accessors dont have matching types."
              pure $ (MkAccessorsE $ AccSimple @t @PBottom pos var)
        _ -> error "impossible case" 
    _ -> error "impossible case"
typeCheckAccessors (L.PLIndexed var [] pos)  = do 
  appendToLog $ "Parsing error at pos " <> T.show pos <> ". Indexes cannot be empty."
  pure $ (MkAccessorsE $ AccSimple @t @PBottom pos var)

typeCheckAccessors (L.PLIndexed var lixs pos) = do 
  MkAccessorsE @sh h <- typeCheckAccessors @t (L.PLId var pos)
  ixs <- forM lixs $ \lix -> do 
    MkEE @a ix <- withExpectedType (Just Z) $ typeCheckExpression lix 
    case decideEquality' @a @PZ of 
      Just Refl -> pure ix 
      Nothing   -> do 
        appendToLog 
          $ "Type error on array index for variable: "
          <> var 
          <> " at "
          <> T.show (L.getExpressionInfo lix)
          <> ". Expected an integer type, but instead got: "
          <> T.show (demote @a)
        pure (EError @PZ @TCCtx $ L.getExpressionInfo lix)

  case sing @sh of 
    STCon (matches @"->" -> Just Refl) 
      (SCons 
        sa 
        (SCons @_ @sb sb@(STCon (matches @"Array" -> Just Refl) _) SNil)
      ) -> withSingI sa $ withSingI sb $ do 
        undefined  
    _ -> undefined


type instance AccSimpleX TCCtx a = AlexPosn 
type instance AccDotX TCCtx a b c = AlexPosn 

-- typeCheckLValue t mSoFar (L.PLIndexed  var lixs pos) = do 
--   env <- ask 
--   -- ask if var is in the accessor space 
--   let mv0  
--         = concatmap (\(a :-> b) -> if a == t then [b] else []) 
--         <$> m.lookup var (tceaccessors env)
--   -- ask if var is a variable
--   mV1 <- yield' var $ tceDict env 
--   ixs <- forM lixs $ \lix -> do 
--     MkEE @a ix <- withExpectedType (Just Z) $ typeCheckExpression lix 
--     case decideEquality' @a @PZ of 
--       Just Refl -> pure ix 
--       Nothing   -> do 
--         appendToLog 
--           $ "Type error on array index for variable: "
--           <> var 
--           <> " at "
--           <> T.show (L.getExpressionInfo lix)
--           <> ". Expected an integer type, but instead got: "
--           <> T.show (demote @a)
--         pure (EError @PZ @TCCtx $ L.getExpressionInfo lix)
--   case ixs of 
--     [] -> do 
--       appendToLog 
--         $ "Syntax error whilst indexing " 
--         <> var 
--         <> " at "
--         <> T.show pos 
--         <> ". Indexes must be non-empty"
--       pure $ (MkLValuableE $ LVId @PBottom pos var)
--     _ -> case (mSoFar, mV0,mV1) of
--       -- var is not found on either space: error 
--       (_,Nothing, Left _)-> do
--         appendToLog $ "Scoping error at " <> T.show pos <> ". Variable " <> var <> " not in scope." 
--         pure $ (MkLValuableE $ LVId @PBottom pos var)
--       -- var is a value: 
--       (Nothing,_,Right (MkAny' @t _)) 
--         -> (\b ta f -> foldlM f b ta ) (MkLValuableE $ LVId @t pos var) ixs 
--         $ \(MkLValuableE @t' acc) ix -> do 
--             case sing @t' of 
--               STCon (matches @"Array" -> Just Refl) (SCons sta SNil) 
--                 -> withSingI sta $ pure $ MkLValuableE $ LvArr pos acc ix 
--               _ -> do
--                 appendToLog 
--                   $ "Type error. Trying to index a non-indexable type at " 
--                   <> T.show pos 
--                   <> " when defining "
--                   <> var 
--                 pure $ (MkLValuableE $ LVId @PBottom pos var)
--       -- var is an accessor. 
--       (Just soFar, Just t,_) ->  undefined
--       _ -> undefined
--
--       
-- typeCheckLValue t mSoFar (L.PLDot var lv pos) = do 
--   env <- ask 
--   -- ask if var is in the accessor space 
--   let mV0  
--         = concatMap (\(a :-> b) -> if a == t then [b] else []) 
--         <$> M.lookup var (tceAccessors env)
--   -- ask if var is a variable
--   mV1 <- yield' var $ tceDict env 
--   case (mSoFar, mV0,mV1) of
--     -- var is not found on either space: error 
--     (_,Nothing, Left _) -> do
--       appendToLog $ "Scoping error at " <> T.show pos <> ". Variable " <> var <> " not in scope." 
--       pure $ (MkLValuableE $ LVId @PBottom pos var)
--     -- var is a value: 
--     (Nothing,_,Right (MkAny' @t _))  -> do 
--       MkLValuableE @lv' lv' <- typeCheckLValue 
--         (demote @t)  
--         (Just $ MkLValuableE $ LVId @t pos var) 
--         lv 
--       pure $ MkLValuableE $ LvDot pos (LVId @t pos var) lv'
--       undefined 
--     _ -> undefined
--


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
type instance EErrorX TCCtx a = AlexPosn 
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


