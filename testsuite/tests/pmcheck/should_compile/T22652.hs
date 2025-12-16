{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror #-}

module T22652 where

data T = Z | S

data ST n where
  SZ :: ST Z
  SS :: ST S

type family F n where
  F Z = Z
  F S = Z

f :: F m ~ n => ST m -> ST n -> ()
f _ SZ = ()
