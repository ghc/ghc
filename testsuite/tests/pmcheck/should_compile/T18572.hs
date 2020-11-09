{-# OPTIONS_GHC -Wincomplete-uni-patterns -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module T18572 where

True = True -- no warning

data SBool (b :: Bool) where
  STrue :: SBool True
  SFalse :: SBool False

STrue = SFalse -- "redundant", not "inaccessible"
