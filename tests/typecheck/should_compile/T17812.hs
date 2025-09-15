{-# LANGUAGE RankNTypes, PolyKinds, KindSignatures #-}
{-# OPTIONS_GHC -fdefer-out-of-scope-variables #-}

module T17812 where
import GHC.Types

bad :: forall (r :: RuntimeRep) a (b :: TYPE r). a -> b
bad x = outOfScope
