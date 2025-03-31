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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

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
import Text.Read (readMaybe)
import Data.List (partition)

data TCCtx

type TypeDict = TypeRepMap PTypes Sing
type ErrLogs = [Text]

data TypeCheckEnv = TCE 
  { tceDict :: TypeDict 
  , tceExpectedType :: Maybe Types
  , tceTypesDict :: Map Text Types  
  , tceAccessors :: Map Text [Types]
  }

iTypeCheckEnv :: TypeCheckEnv 
iTypeCheckEnv  = TCE 
  { tceDict = empty 
  , tceExpectedType = Nothing 
  , tceTypesDict = M.empty 
  , tceAccessors = M.empty
  }

iErrLogs :: ErrLogs
iErrLogs = []

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

newtype TCM a = TCM 
  { runTCM :: ReaderT TypeCheckEnv (WriterT ErrLogs IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader TypeCheckEnv, MonadWriter ErrLogs, MonadIO)

runTCMIO :: TCM a -> IO (a,ErrLogs)
runTCMIO tcm = runWriterT $ runReaderT (runTCM tcm) iTypeCheckEnv 


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
  => L.FunArg AlexPosn -> m (TypeCheckEnv, FunctionArg TCCtx)
typeCheckFunArg (L.FunArg t var mode pos) = do 
  env <- ask 
  let d = tceDict env
  t' <- typeCheckTypes t
  SomeSing @_ @st st <- pure $ toSing t' 
  withSingI st $ do 
    d' <- insertFresh var st d

    pure 
      (env{tceDict = d'},MkFunctionArg @st @TCCtx pos var $ maybe POValue (const PORef) mode)

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

genNewFunctions ::   ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) 
  => Text -> Types -> m TypeCheckEnv 
genNewFunctions fName (Record (xs)) = do 
  env <- ask 
  let xs' = fmap (\(_ :~.: t') -> t') xs
  let reT = ZId fName 
  let t' = foldr (\next acc -> next :-> acc) reT xs'
  SomeSing  snewF <- pure $ toSing t' 
  d <- withSingI snewF $ insertFresh ("new<" <> fName <> ">") snewF $ tceDict env
  pure $ env{tceDict=d} 
genNewFunctions _ _ = error "impossible case"

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
      Right dict' -> pure env'{tceDict = dict'}
      Left _ -> error "impossible case"
  -- update the accessors. 
  let env2 = env1{tceAccessors = M.unionsWith mappend [tceAccessors env1, genAccessors t `runReader` ZId tname] }
  -- update new functions 
  env3 <- local (const env2) $ genNewFunctions tname  t
  liftIO $ putStrLn $ T.unpack tname
  liftIO $ print $ M.toList $ tceTypesDict env3 
  pure (env3, TypeDef pos tname t)
typeCheckDef (L.FunctionDef reT fname args as pos) = do
  env     <- ask
  !reT'    <- typeCheckTypes reT 
  !funType <- generateFunType reT' args 
  (newEnv,funArgs) <- (\b ta f -> foldlM f b ta) (env,[])  args $ \(e,acc) arg -> do 
    (newEnv,targ) <- local (const e) $ typeCheckFunArg arg 
    pure (newEnv,acc <> [targ])
  SomeSing @_ @sFunType sFunType <- pure $ toSing funType 
  SomeSing @_ @sRet sRet <- pure $ toSing reT'


  withSingI sFunType 
    $ withSingI sRet 
    $ local (const newEnv)
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
          $ typeCheckActions pos as 
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


typeCheckDefs :: forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) 
  => [L.Definition AlexPosn] -> m [(TypeCheckEnv,Definition TCCtx)]
typeCheckDefs xs = ask >>= \env -> fmap snd $ (\b ta f -> foldlM f b ta) (env,[]) xs $ \(e,acc) def -> do 
  (newEnv, d) <- local (const e) $ typeCheckDef def
  pure (newEnv, (newEnv,d) : acc)

typeCheckAction ::  forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) => L.Action AlexPosn -> m (TypeCheckEnv, AEF TCCtx)
typeCheckAction (L.Declare lt var Nothing) = do 
  env <- ask
  t   <- typeCheckTypes lt 
  let pos = L.getPTypesInfo lt
  appendToLog $ "Parsing Error at " <> T.show pos <> " variable " <> var <> " must be initialized."
  SomeSing @_ @st st <- pure $ toSing t
  withSingI st 
    $ withDeclared @st var (pure (env,MkAEF $ ABottom @'[] @PAUnit pos))
    $ ask >>= \env' -> pure (env', MkAEF $ ABottom @'[] @PAUnit pos)

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
        pure (env', MkAEF $  Declare pos (LVId @st pos var) $ EError @st pos)
typeCheckAction (L.Assign lv e) = do 
  let pos = L.getLValueInfo lv
  MkLValuableE @lv' lv' <- typeCheckLValue lv
  MkEE @e' e' <- typeCheckExpression e
  env <- ask 
  case decideEquality' @lv' @e' of 
    Just Refl -> pure (env, MkAEF $ Assignment pos lv' e')
    _ -> do 
      appendToLog  
        $ "Type mismatch on the reassignment for: " 
        <> T.show lv 
        <> ", at: "
        <> T.show pos 
        <> ". Expected type: "
        <> T.show (demote @lv')
        <> ", but instead got: "
        <> T.show (demote @e')
        <> "."
      pure (env, MkAEF $ ABottom @'[] @PAUnit pos)

typeCheckAction (L.For pa e las pos) = do 
  env <- ask
  MkEE @e' e' <- withExpectedType Nothing $ typeCheckExpression e 
  MkPatternE @pe pe <- snd <$> typeCheckPattern (demote @e') pa
  MkAEF las' <- snd <$> typeCheckLoopActions pos las
  case decideEquality' @e' @pe of 
    Just Refl -> pure (env, MkAEF $ ForLoop pos pe e' las') 
    Nothing -> pure (env, MkAEF $ ABottom @'[] @PAUnit pos)
typeCheckAction (L.While e as pos) = do 
  env <- ask 
  MkEE @e' e' <- withExpectedType (Just ZBool) $ typeCheckExpression e
  MkAEF as' <- snd <$> typeCheckLoopActions pos as
  case decideEquality' @e' @PZBool of 
    Just Refl -> pure (env,MkAEF $ WhileLoop pos e' as')
    Nothing   -> case tceExpectedType env of 
      Just (toSing -> SomeSing @_ @et et) -> withSingI et $ pure (env,MkAEF $ ABottom @'[] @et pos)
      Nothing -> pure (env,MkAEF $ ABottom @'[] @PAUnit pos)
typeCheckAction (L.AExpression e) = do 
  let pos = L.getExpressionInfo e
  env <- ask 
  MkEE e' <- typeCheckExpression e
  pure (env, MkAEF $ AE pos e')
typeCheckAction (L.Return e pos) = ask >>= \env -> do 
  MkEE e' <- typeCheckExpression e 
  pure (env, MkAEF $ AReturn pos e' )


type instance DeclX TCCtx a = AlexPosn 
type instance LvIdX TCCtx a = AlexPosn 
type instance AssX TCCtx a = AlexPosn 
type instance ABotX TCCtx  = AlexPosn
type instance AEX TCCtx a = AlexPosn
type instance ASX TCCtx a = AlexPosn
type instance ForX TCCtx actx a b = AlexPosn 
type instance SkipX TCCtx a = ()
type instance WhileX TCCtx actx a = AlexPosn 
type instance AReturnX TCCtx a = AlexPosn 

typeCheckLoopAction :: forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) => L.LoopAction AlexPosn -> m (TypeCheckEnv,AEF TCCtx)
typeCheckLoopAction (L.Break pos) = ask >>= \env -> pure (env,MkAEF $ ABreak pos)
typeCheckLoopAction (L.Continue pos) = ask >>= \env -> pure  (env, MkAEF $ AContinue pos)
typeCheckLoopAction (L.LAction a) = typeCheckAction a

typeCheckLoopActions :: forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) => AlexPosn -> [L.LoopAction AlexPosn] -> m (TypeCheckEnv,AEF TCCtx)
typeCheckLoopActions pos [] = ask >>= \env -> case tceExpectedType env of 
  Just (toSing -> SomeSing @_ @et et) -> withSingI et $ do 
    appendToLog 
      $ "Type error at " <> T.show pos <> ".  Expected type: " <> T.show (tceExpectedType env) 
      <> " But instead got an empty sequence of actions."
    pure (env, MkAEF $ ABottom @'[] @et pos) 
  Nothing -> pure (env, MkAEF $ Skip @PAUnit ()) 
typeCheckLoopActions _ (la:las) = do 
  let lapos = L.getLoopActionInfo la 
  (newEnv', MkAEF @actxla'  la') <-  typeCheckLoopAction la 
  (\ b ta f -> foldlM f b ta) (newEnv',MkAEF @actxla' $ AS lapos (MkAECtx @actxla'  la') []) las 
    $ \(env',MkAEF @actx (AS  _ (MkAECtx x) xs)) a -> do
      let lapos' = L.getLoopActionInfo a 
      (currEnv,MkAEF @actx' @curr curr) <- local (const env') $ typeCheckLoopAction a
      SomeSing @_ @factx factx <- pure . toSing $ defineActionCtx (demote @actx) (demote @actx')
      withSingI factx 
       $ pure (currEnv, MkAEF $ AS @factx lapos' (MkAECtx curr) $ MkAEF x : xs)
  






type instance ABreakX TCCtx = AlexPosn 
type instance AContinueX TCCtx = AlexPosn 


typeCheckActions :: forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) => AlexPosn -> [L.Action AlexPosn] -> m (TypeCheckEnv, AEF TCCtx)
typeCheckActions pos [] = ask >>= \env -> case tceExpectedType env of 
  Just (toSing -> SomeSing @_ @et et) -> withSingI et $ do 
    appendToLog 
      $ "Type error at " <> T.show pos <> ".  Expected type: " <> T.show (tceExpectedType env) 
      <> " But instead got an empty sequence of actions."
    pure (env, MkAEF $ ABottom @'[] @et pos) 
  Nothing -> pure (env, MkAEF $ Skip @PAUnit ()) 
typeCheckActions pos (la:las) = do
  let lapos = L.getActionInfo la 
  (newEnv', MkAEF @actxla'  la') <-  typeCheckAction la 
  (\ b ta f -> foldlM f b ta) (newEnv',MkAEF @actxla' $ AS lapos (MkAECtx @actxla'  la') []) las 
    $ \(env',MkAEF @actx (AS  _ (MkAECtx x) xs)) a -> do
      let lapos' = L.getActionInfo a 
      (currEnv,MkAEF @actx' @curr curr) <- local (const env') $ typeCheckAction a
      SomeSing @_ @factx factx <- pure . toSing $ defineActionCtx (demote @actx) (demote @actx')
      withSingI factx 
       $ pure (currEnv, MkAEF $ AS @factx lapos' (MkAECtx curr) $ MkAEF x : xs)
  


data PatternE where 
  MkPatternE :: SingI a => Pattern TCCtx a -> PatternE

type instance PaBX TCCtx = AlexPosn 

typeCheckPattern :: forall m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  ) 
  => Types -> L.Pattern AlexPosn -> m (TypeCheckEnv, PatternE)
typeCheckPattern _ (L.PaBool "true" p) = ask >>= \env -> pure (env,MkPatternE $ PaB p True)
typeCheckPattern _ (L.PaBool "false" p) = ask >>= \env -> pure (env,MkPatternE $ PaB p False)
typeCheckPattern _ (L.PaBool _ p) = ask >>= \env -> do 
  appendToLog $ "Parse error at " <> T.show p <> ". Bools must be either \"true\" or \"false\"."
  pure (env,MkPatternE $ PaErr @PZBool p)

typeCheckPattern _ (L.PaNumber t pos) = case readMaybe @Int $ T.unpack t of 
  Just n  -> ask >>= \env -> pure (env, MkPatternE $ PaZ pos n)
  Nothing -> ask >>= \env -> do 
    appendToLog 
      $ "Pattern error at " <> T.show pos <> ", Floating numbers cannot be pattern matched on."
    pure (env, MkPatternE $ PaErr @PZ pos)
typeCheckPattern _ (L.PaString t pos) = ask >>= \env -> pure (env, MkPatternE $ PaString pos t)
typeCheckPattern _ (L.PaChar t pos) = case t of 
  T.Empty -> ask >>= \env -> do 
    appendToLog 
      $ "Pattern error at " <> T.show pos <> ", Character literals must be non-empty"
    pure (env, MkPatternE $ PaErr @PZChar pos)
  a T.:< T.Empty -> ask >>= \env -> pure (env, MkPatternE $ PaChar pos a)
  _ ->  ask >>= \env -> do 
    appendToLog 
      $ "Pattern error at " <> T.show pos <> ", Character literals must be non-empty"
    pure (env, MkPatternE $ PaErr @PZChar pos)
typeCheckPattern et (L.PaId t mode pos) = do 
  env <- ask 
  SomeSing @_ @set set <- pure $ toSing et 
  withSingI set $ do 
    e' <- insertFresh t set $ tceDict env
    let m = maybe POValue (const PORef) mode
    pure (env{tceDict=e'}, MkPatternE $ PaId @set pos t m)

typeCheckPattern (ZId a) pt@(L.PaPattern _) = do 
  env <- tceTypesDict <$> ask 
  case M.lookup a env of 
    Just t ->  typeCheckPattern t pt 
    _ -> error "Impossible case. TypeCheckPattern must always receive a valid type."


typeCheckPattern (Record rts) (L.PaPattern (L.RecordPattern (n,lt,p0) tltps pos)) = do 
  t <- typeCheckTypes lt
  ts <- traverse (\(a,b,c) -> (a,,c) <$> typeCheckTypes b) tltps
  ds <- f rts $ (n, t, p0) : ts
  e <- tceDict <$> ask
  e' <- (\b ta f -> foldlM f b ta) e ds $ \env (n,lt',_) -> do 
    SomeSing sn <- pure $ toSing n
    SomeSing slt' <- pure $ toSing lt'
    withSingI sn $ withSingI slt' $ insertFresh n slt' env 
  env <- ask
  SomeSing @_ @srts srts <- pure $ toSing rts 
  withSingI srts $ 
    pure (env{tceDict = e'},MkPatternE $ PaRecord @(PRecord srts) pos (Record rts))

  where 
  f :: [Types] -> [(Text,Types,AlexPosn)] -> m [(Text,Types,AlexPosn)]
  f _ [] = pure []
  f ts (x:xs) =  do 
   (ts',mx) <- g ts  x 
   case mx of 
    Nothing -> f ts' xs 
    Just x' -> (x : ) <$> f ts' xs

  g :: [Types] -> (Text,Types,AlexPosn) -> m ([Types],Maybe (Text,Types,AlexPosn))
  g ts (var,t,pos) = do 
    let (s,rest) = Data.List.partition (== (var :~.: t)) ts
    case s of 
      [] -> do 
        appendToLog 
          $  "Pattern variable: " <> var <> " at " 
          <> T.show pos <> " is not an accessor for type "
          <> T.show (Record ts )
        pure (ts,Nothing)
      (_:_) -> pure (rest,Just (var,t,pos))

typeCheckPattern t (L.PaPattern (L.RecordPattern _ _ pos) ) = do 
  env <- ask
  appendToLog $ "Type error. Type: " <> T.show t <> " has no patterns."
  SomeSing @_ @st st <- pure $ toSing t
  withSingI st $ 
    pure (env, MkPatternE $ PaErr @st pos)


type instance PaZX TCCtx = AlexPosn 
type instance PaErrX TCCtx a = AlexPosn 
type instance PaStringX TCCtx = AlexPosn
type instance PaCharX TCCtx = AlexPosn 
type instance PaIdX TCCtx a = AlexPosn 
type instance PaRecordX TCCtx a = AlexPosn 

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
  MkAccessorsE lv' <- typeCheckAccessors @t lv 
  pure $ MkLValuableE $ LVIdWithAcc pos var lv' 

typeCheckLValue l@(L.PLIndexed var _ pos) = do 
  env <- ask 
  mv <- yield' var $ tceDict env 
  case mv of 
    Left e -> do 
      appendToLog $ "Scoping error at " <> T.show pos <> ". " <> showGammaErrors e 
      pure (MkLValuableE $ LVId @PBottom pos var)
    Right (MkAny' @st _) -> do 
      MkAccessorsE  lv' <- local (\env' -> 
        env'{tceAccessors= M.unionWith mappend 
          (M.singleton var [demote @st :-> demote @st])
          (tceAccessors env')
          }) $ typeCheckAccessors @st l
      pure $ MkLValuableE $ LVIdWithAcc pos var lv'

  



type instance LvIdWithAccX TCCtx a b = AlexPosn

data AccessorsE base where 
  MkAccessorsE :: forall a base. SingI a => Accessors TCCtx (base :~> a) -> AccessorsE base 

typeCheckAccessors :: forall (t :: PTypes) m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  , SingI t -- type of the parent. i.e: a.b => t ~ a
  ) =>  L.LValuable AlexPosn -> m (AccessorsE t)
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
  MkAccessorsE t <- typeCheckAccessors @sh lv 
  pure $ MkAccessorsE $ AccDot pos h t
typeCheckAccessors (L.PLIndexed var [] pos)  = do 
  appendToLog $ "Parsing error at pos " <> T.show pos <> ". Indexes cannot be empty."
  pure $ (MkAccessorsE $ AccSimple @t @PBottom pos var)

typeCheckAccessors (L.PLIndexed var lixs pos) = do 
  tvar <- typeCheckAccessors @t (L.PLId var pos)
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

  (\b ta f -> foldlM f b ta) tvar ixs $ \(MkAccessorsE @sh h) ix -> do 
    case sing @sh of 
      STCon (matches @"Array" -> Just Refl) (SCons sc SNil) -> withSingI sc $ do 
            pure $ MkAccessorsE $ AccArr pos h ix 
      _ -> do 
        appendToLog $ "Type error indexing: " <> T.show h <> ". Can only index vector types."
        pure $ (MkAccessorsE $ AccSimple @t @PBottom pos var)


type instance AccSimpleX TCCtx a = AlexPosn 
type instance AccDotX TCCtx a b c = AlexPosn 
type instance AccArrX TCCtx a b = AlexPosn 

data EE where 
  MkEE :: SingI a => E TCCtx a -> EE 

typeCheckExpression :: 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  )
  => L.Expression AlexPosn 
  -> m EE 
typeCheckExpression le@(L.Plus l r pos) 
  = typeCheckBinOp
    @PZ
    @PF
    @PF
    @PF 
    l r pos 
    EPlusZ
    EPlusF 
    EPlusZF
    EPlusFZ
typeCheckExpression (L.ENumber t pos) = do 
  env <- ask 
  case tceExpectedType env of 
    Just Z -> case readMaybe @Int $ T.unpack t of 
      Just n -> pure . MkEE $ EZ pos n
      Nothing -> do 
        appendToLog 
          $ "Type error at: " <> T.show pos <> ".Number constant cannot be coerced to integer type."
        pure . MkEE $ EError @PZ pos
    Just F -> case readMaybe @Float $ T.unpack t of 
      Just n -> pure . MkEE $ EF pos n
      Nothing -> do 
        appendToLog 
          $ "Type error at: " <> T.show pos <> ".Number constant cannot be coerced to floating type."
        pure . MkEE $ EError @PZ pos
    Nothing -> case (readMaybe @Int $ T.unpack t, readMaybe @Float $ T.unpack t) of 
      (Just n,_) -> pure . MkEE $ EZ pos n
      (_,Just n) -> pure . MkEE $ EF pos n
      _ -> do 
        appendToLog 
          $ "Type error at: " <> T.show pos <> ". Malformed number constant."
        pure . MkEE $ EError @PZ pos
    Just et -> do 
      SomeSing @_ @set set <- pure $ toSing et 
      appendToLog 
        $ "Type error at: " <> T.show pos <> ". Expected type: " <> T.show et <> ", but got a number instead."
      withSingI set $ 
        pure . MkEE $ EError @set pos
typeCheckExpression (L.EString s pos) = ask >>= \env -> case tceExpectedType env of 
  Just ZString -> pure . MkEE $ EZString pos s 
  Nothing -> pure . MkEE $ EZString pos s 
  Just et -> do 
    SomeSing @_ @set set <- pure $ toSing et
    appendToLog 
      $ "Type error at: " <> T.show pos <> ". Expected type: " <> T.show et <> ", but got a string constant instead."
    withSingI set $ 
      pure . MkEE $ EError @set pos


typeCheckExpression (L.EChar s pos) = case s of 
  T.Empty -> do 
    appendToLog 
      $ "Syntax error at " <> T.show pos <> ", character literals must be non-empty."
    pure . MkEE $ EError @PZChar pos 
  a T.:< T.Empty -> ask >>= \env -> case tceExpectedType env of 
    Just ZChar -> pure . MkEE $ EZChar pos a
    Nothing -> pure . MkEE $ EZChar pos a 
    Just et -> do 
      SomeSing @_ @set set <- pure $ toSing et
      appendToLog 
        $ "Type error at: " <> T.show pos <> ". Expected type: " <> T.show et <> ", but got a character constant instead."
      withSingI set $ 
        pure . MkEE $ EError @set pos

  _ -> do 
    appendToLog 
      $ "Syntax error at " <> T.show pos <> ", character literals must have 1 character"
    pure . MkEE $ EError @PZChar pos 
typeCheckExpression (L.EBool t pos) = ask >>= \env -> case tceExpectedType env of 
  Just ZBool -> f t  
  Nothing -> f t 
  Just et -> do 
    SomeSing @_ @set set <- pure $ toSing et
    appendToLog 
      $ "Type error at: " <> T.show pos <> ". Expected type: " <> T.show et <> ", but got a boolean constant instead."
    withSingI set $ 
      pure . MkEE $ EError @set pos
  where 
    f "true" = pure . MkEE $ EZBool pos True 
    f "false" = pure . MkEE $ EZBool pos False
    f _ = do 
      appendToLog 
        $ "Syntax error at " <> T.show pos <> ". Bools can only be either true or false."
      pure . MkEE $ EError @PZBool pos
typeCheckExpression (L.Times l r pos) 
  = typeCheckBinOp
    @PZ
    @PF
    @PF
    @PF 
    l r pos 
    ETimesZ
    ETimesF 
    ETimesZF
    ETimesFZ
typeCheckExpression (L.Minus l r pos) 
  = typeCheckBinOp
    @PZ
    @PF
    @PF
    @PF 
    l r pos 
    EMinusZ
    EMinusF 
    EMinusZF
    EMinusFZ

typeCheckExpression (L.ELValuable (L.PLId var pos)) = do 
  env <- ask 
  mv <- yield' var $ tceDict env 
  case tceExpectedType env of 
    Just t -> do 
      SomeSing @_ @et et <- pure $ toSing t 
      withSingI et $ case mv of 
        Left e -> do 
          appendToLog 
            $  "Scoping error at " <> T.show pos <> ": "
            <> showGammaErrors e
          pure . MkEE $ EError @et pos
        Right (MkAny' sv) -> case decideEquality et sv of 
          Just Refl -> pure . MkEE $ EVar @et pos var 
          Nothing -> do 
            appendToLog 
              $ "Type error at " <> T.show pos <> ". The expected type for the variable "
              <> var 
              <> " is: "
              <> T.show t 
              <> ", but instead got: "
              <> T.show (fromSing sv)
            pure . MkEE $ EError @et pos
    Nothing ->  case mv of 
      Left e -> do 
        appendToLog 
          $  "Scoping error at " <> T.show pos <> ": "
          <> showGammaErrors e
        pure . MkEE $ EError @PBottom pos
      Right (MkAny' @sv _) -> pure . MkEE $ EVar @sv pos var
typeCheckExpression (L.FApp var [] pos) = undefined
typeCheckExpression (L.FApp var xs pos) = do 
  env <- ask 
  MkEE @f f <-  withExpectedType Nothing  
    $ typeCheckExpression (L.ELValuable (L.PLId var pos))
  case sing @f of 
    STCon (matches @"->" -> Just Refl) (SCons @_ @sa sa (SCons @_ @sb sb SNil))
      ->  withSingI sa $ withSingI sb $ typeCheckFAppS f xs 
    _ -> do 
      appendToLog 
        $ "Type error in function application, at: "
        <> T.show pos 
        <> ", Variable "
        <> var 
        <> " is not a function."
      case tceExpectedType env of 
        Just (toSing -> SomeSing @_ @et et) -> withSingI et $ pure . MkEE $ EError @et pos 
        Nothing ->  pure . MkEE $ EError @PBottom pos 

typeCheckExpression (L.New (L.PId var _) xs pos) = do 
  liftIO $ putStrLn $ T.unpack var
  typeCheckExpression  (L.FApp  ("new<" <> var <> ">") xs pos )
  
  
typeCheckExpression _ = undefined


type instance EAppX TCCtx a b = AlexPosn 
type instance EVarX TCCtx a = AlexPosn
type instance EPlusZX TCCtx = AlexPosn 
type instance EPlusFX TCCtx = AlexPosn 
type instance EBotX   TCCtx = AlexPosn 
type instance EErrorX TCCtx a = AlexPosn 
type instance EPlusZFX TCCtx = AlexPosn 
type instance EPlusFZX TCCtx = AlexPosn 
type instance EZX TCCtx = AlexPosn
type instance EFX TCCtx = AlexPosn
type instance EZStringX TCCtx = AlexPosn
type instance EZCharX TCCtx = AlexPosn 
type instance EZBoolX TCCtx = AlexPosn
type instance ETimesZX TCCtx = AlexPosn 
type instance ETimesFX TCCtx = AlexPosn 
type instance ETimesZFX TCCtx = AlexPosn 
type instance ETimesFZX TCCtx = AlexPosn 
type instance EMinusZX TCCtx = AlexPosn 
type instance EMinusFX TCCtx = AlexPosn 
type instance EMinusZFX TCCtx = AlexPosn 
type instance EMinusFZX TCCtx = AlexPosn 



typeCheckFAppS :: forall a b m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  , SingI a 
  , SingI b
  ) 
  => E TCCtx (a :~> b)
  -> [L.Expression AlexPosn]
  -> m EE 
typeCheckFAppS f xs = (\b ta op -> foldlM op b ta) (MkEE f) xs $ \(MkEE @uf uf) arg -> do 
  let pos = L.getExpressionInfo arg 
  case sing @uf of 
    STCon (matches @"->" -> Just Refl) (SCons @_ @sa sa (SCons @_ @sb sb SNil))
      -> withSingI sa $ withSingI sb $ do 
        liftIO $ print $ demote @uf
        MkEE @targ targ <- withExpectedType (Just $ fromSing sa) $ typeCheckExpression arg
        case decideEquality' @targ @sa of 
          Just Refl -> pure . MkEE $ EApp pos uf targ
          Nothing -> do 
            appendToLog 
              $ "Type error in function application, at: "
              <> T.show pos 
              <> ". Expected type: "
              <> T.show (demote @sa)
              <> "..., but instead got: "
              <> T.show (demote @targ)
            pure . MkEE $ EError @sb pos 
    _ -> do  
      appendToLog 
        $ "Type error in function application, at: "
        <> T.show pos 
        <> ". Not enough arguments to apply the function."
      pure . MkEE $ EError @uf pos 


 


typeCheckBinOp :: forall x0 x1 x2 x3 m. 
  ( MonadReader TypeCheckEnv m 
  , MonadWriter ErrLogs m
  , MonadIO m 
  , SingI x0
  , SingI x1
  , SingI x2
  , SingI x3
  )
  => L.Expression AlexPosn 
  -> L.Expression AlexPosn 
  -> AlexPosn 
  -> (AlexPosn -> E TCCtx PZ -> E TCCtx PZ -> E TCCtx x0)
  -> (AlexPosn -> E TCCtx PF -> E TCCtx PF -> E TCCtx x1)
  -> (AlexPosn -> E TCCtx PZ -> E TCCtx PF -> E TCCtx x2)
  -> (AlexPosn -> E TCCtx PF -> E TCCtx PZ -> E TCCtx x3)
  -> m EE 
typeCheckBinOp l r pos f0 f1 f2 f3 = do 
  MkEE @l' l' <- withExpectedType Nothing $ typeCheckExpression l
  MkEE @r' r' <- withExpectedType Nothing $ typeCheckExpression r 
  case (sing @l', sing @r') of 
    ( STCon (matches @"Z" -> Just Refl) SNil
      , STCon (matches @"Z" -> Just Refl) SNil 
      ) -> pure . MkEE $ f0 pos l' r'
    ( STCon (matches @"F" -> Just Refl) SNil
      , STCon (matches @"F" -> Just Refl) SNil 
      ) -> pure . MkEE $ f1 pos l' r'
    ( STCon (matches @"Z" -> Just Refl) SNil
      , STCon (matches @"F" -> Just Refl) SNil
      ) -> pure . MkEE $ f2 pos l' r'
    ( STCon (matches @"F" -> Just Refl) SNil
      , STCon (matches @"Z" -> Just Refl) SNil 
      ) -> pure . MkEE $ f3 pos l' r'
    (STCon (matches @"Bottom" -> Just Refl) _, _) 
      -> pure $ MkEE l'
    (_,STCon (matches @"Bottom" -> Just Refl) _) 
      -> pure $ MkEE r'
    _ -> do 
      appendToLog 
        $ "Type Error. Incoercible types encountered at position: " 
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
  -- nEnv0 <- f <$> declare @x v eEnv 
  nEnv <-  TypedMap.insert v (sing @x) eEnv
  case nEnv of 
    Left e -> appendToLog (showGammaErrors e) >> iError 
    Right nEnv' -> local (\env' -> env'{tceDict=nEnv'}) ma 
  where   
    f (Right a ) = a 

