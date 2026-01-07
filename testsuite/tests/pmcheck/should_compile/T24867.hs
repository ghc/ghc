{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}
{-# OPTIONS_GHC -Winaccessible-code -Werror #-}

module T24867 where

data T = Z | S

data ST n where
  SS :: ST S

type family F n where
  F Z = Z
  F S = Z

-- Should be rejected with inaccessible RHS
f :: F m ~ n => ST m -> ST n -> ()
f _ SS = ()
