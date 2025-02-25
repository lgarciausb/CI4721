{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Parser.ScopeCheck where

import Parser.LexerDefinitions 
import Parser.Lexer
import Control.Monad.Reader 
import Control.Monad.Writer
import TypedMap 
import Data.Text (Text)
import Data.Text qualified as T
import Control.Monad 
import Data.Foldable

data Void1 (a :: ())

type ScopeDict = TypeRepMap () Void1
type ErrLogs = [Text]

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

class Scope a where 
  checkScoping :: (MonadReader ScopeDict m, MonadWriter ErrLogs m, MonadIO m) => a -> m ScopeDict

instance Scope (RecordPattern AlexPosn) where
  checkScoping (RecordPattern (v,t,_) xs _) = checkScoping (t : [a | (_,a,_) <- xs]) >> do 
    env0 <- ask 
    foldM (flip $ declareFresh @'() @Void1) env0 $ v : [a | (a,_,_) <- xs]
   


instance Scope (PTypes AlexPosn) where
  checkScoping (PAtom v (AlexPn _ l c)) = do 
    e0 <- ask 
    let m0 = v `notInScope` e0
    when m0 $ appendToLog 
      ( "Scope error at line " 
      <> T.show l 
      <> ", column " 
      <> T.show c
      <> ". While parsing the atom: "
      <> v
      <> "; Atom not in scope."
      )
    pure e0

  checkScoping (PId v (AlexPn _ l c)) = do 
    e0 <- ask 
    let m0 = v `notInScope` e0
    when m0 $ appendToLog 
      ( "Scope error at line " 
      <> T.show l 
      <> ", column " 
      <> T.show c
      <> ". While parsing the type: "
      <> v
      <> "; Type not in scope."
      )
    pure e0

  checkScoping (PUnion a b) = checkScoping a *> checkScoping b
  checkScoping (PRecord x) = checkScoping x

instance Scope (FunArg AlexPosn) where
  checkScoping (FunArg t v _ (AlexPn _ l c)) = do 
    _ <- checkScoping t 
    e0 <- ask 
    let m0 = v `inScope` e0
    when m0 $ appendToLog 
      ( "Scope error at line: "
      <> T.show l
      <> ", column: "
      <> T.show c
      <> ", at the function parameter: "
      <> v
      <> ". Symbol already defined."
      )
    declareFresh @'() @Void1 v e0


instance Scope (Definition AlexPosn) where
  checkScoping (FunctionDef t v fs as (AlexPn _ l _)) = do 
    _ <-  checkScoping t 
    e0 <- ask 
    let m0 = v `inScope` e0
    when m0 $ appendToLog 
      ( "Scope error at line: "
      <> T.show l
      <> ", at the declaration of the function: "
      <> v
      <> ". Symbol already defined."
      )
    e1 <- declareFresh @'() @Void1 v e0 >>= \e0' -> local (const e0') $ checkScoping fs
    local (const e1) $ checkScoping as


instance Scope (LValuable AlexPosn) where
  checkScoping (PLId v (AlexPn _ l c)) = ask >>= \d -> case v `inScope` d of
    True -> pure d
    False -> appendToLog 
      ( "Scope error at line " 
      <> T.show l 
      <> ", column " 
      <> T.show c
      <> ". Identifier: "
      <> v
      <> " not defined"
      ) *> ask
  checkScoping (PLIndexed v xs p) = checkScoping (PLId v p) *> checkScoping xs
  checkScoping (PLDot v l p) = checkScoping (PLId v p) *> checkScoping l

instance Scope (Pattern AlexPosn) where
  checkScoping (PaId t _ _) = ask >>= declareFresh @'() @Void1 t
  checkScoping (PaPattern x) = checkScoping x
  checkScoping _ = ask

instance Scope (Action AlexPosn) where
  checkScoping (For x xs as _) 
    = checkScoping xs 
    >> checkScoping x 
    >>= \e -> local (const e) $ checkScoping as  
  checkScoping (While e as _) = checkScoping e >> checkScoping as 
  checkScoping (Assign x e) = checkScoping x >> checkScoping e 
  checkScoping (AExpression e) = checkScoping e 
  checkScoping (Return e _) = checkScoping e

instance Scope (LoopAction AlexPosn) where 
  checkScoping (LAction a) = checkScoping a
  checkScoping _ = ask

instance Scope (Expression AlexPosn) where
  checkScoping (ELValuable e) = checkScoping e
  checkScoping (New t es _) = checkScoping t >> checkScoping es
  checkScoping (Ref x _) = checkScoping x
  checkScoping (Neg x _) = checkScoping x
  checkScoping (Plus a b _) = checkScoping a >> checkScoping b
  checkScoping (Times a b _) = checkScoping a >> checkScoping b
  checkScoping (Divide a b _) = checkScoping a >> checkScoping b
  checkScoping (Power a b _) = checkScoping a >> checkScoping b
  checkScoping (Mod a b _) = checkScoping a >> checkScoping b
  checkScoping (Minus a b _) = checkScoping a >> checkScoping b
  checkScoping (ELT a b _) = checkScoping a >> checkScoping b
  checkScoping (EGT a b _) = checkScoping a >> checkScoping b 
  checkScoping (NotEq a b _) = checkScoping a >> checkScoping b
  checkScoping (EEq a b _) = checkScoping a >> checkScoping b
  checkScoping (EGTEq a b _) = checkScoping a >> checkScoping b
  checkScoping (ELTEq a b _) = checkScoping a >> checkScoping b 
  checkScoping (Or a b _) = checkScoping a >> checkScoping b
  checkScoping (And a b _) = checkScoping a >> checkScoping b 
  checkScoping (Not a _) = checkScoping a 
  checkScoping (Arr es _) = checkScoping es 
  checkScoping (Match e pas _) = checkScoping e 
    >> traverse_ (\(p,as) -> checkScoping p >>= \d -> local (const d) $ checkScoping as) pas
    >> ask
  checkScoping (FApp f args (AlexPn _ l c)) = ask >>= \env0 -> do 
    when (f `notInScope` env0) $ appendToLog
      ( "Scope error at line " 
      <> T.show l 
      <> ", column " 
      <> T.show c
      <> ". Function Identifier: "
      <> f
      <> " not defined"
      )
    declareFresh @'() @Void1 f env0 >>= \env1 -> local (const env1) $ checkScoping args
  checkScoping _ = ask 


instance Scope a => Scope [a] where
  checkScoping xs = ask >>= \d -> foldM (\d' a -> local (const d') . checkScoping $ a ) d xs 

instance (Scope a,Scope b) => Scope (a,b) where  
  checkScoping (a,b) = checkScoping a >>= \d -> local (const d) $ checkScoping b

runScopeIO :: [Definition AlexPosn] -> IO ErrLogs
runScopeIO xs = execWriterT $ runReaderT (checkScoping  xs) empty
