{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module VisibleDependentQuantificationFail3 where

pattern Nil :: forall a -> [a]
pattern Nil <- undefined
