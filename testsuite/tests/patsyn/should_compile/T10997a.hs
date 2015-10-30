{-# LANGUAGE GADTs, PatternSynonyms #-}

module T10997a where

data Exp ty where
  LitB :: Bool -> Exp Bool

pattern Tru :: () => b ~ Bool => Exp b
pattern Tru = LitB True


