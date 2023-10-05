{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T16326_Fail3 where

pattern Nil :: forall a -> [a]
pattern Nil <- undefined
