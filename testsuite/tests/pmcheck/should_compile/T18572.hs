{-# OPTIONS_GHC -Wincomplete-uni-patterns -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module T18572 where

True = True

data SBool (b :: Bool) where
  STrue :: SBool True
  SFalse :: SBool False

STrue = SFalse
