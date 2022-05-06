{-# LANGUAGE DataKinds, KindSignatures, ExplicitForAll #-}
module T where

import GHC.Exts

a = let x :: forall (a :: TYPE IntRep). a
        x = error ""
    in ()
