{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Test.BuiltinOverride (main) where

import Control.Concurrent
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule
import Test.Type

newtype Key = Key Int
  deriving (Show, Eq, Hashable, Binary, NFData, Typeable)

type instance RuleResult Key = ()

main sleep = do
  store <- newEmptyMVar

  testBuild (test store) (setRules store) sleep

setRules resultsStore = do
  addBuiltinRule noLint noIdentity $ \(Key n) _ _ -> do
    liftIO $ putMVar resultsStore n
    pure $ RunResult ChangedRecomputeDiff mempty ()
  addBuiltinRule noLint noIdentity $ \(Key n) _ _ -> do
    liftIO $ putMVar resultsStore (n + 1)
    pure $ RunResult ChangedRecomputeDiff mempty ()
  action $ apply1 $ Key 1

test store build = do
  build ["--allow-redefine-rules"]

  res <- takeMVar store
  assertBool (res == 2) "Rule was not overridden"

  assertException ["rule defined twice"] $ build ["--quiet"]
