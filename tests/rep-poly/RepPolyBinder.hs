{-# LANGUAGE ViewPatterns #-}

module RepPolyBinder where

import GHC.Exts

myId :: forall rep (x :: TYPE rep). x -> x
myId = undefined

foo :: forall rep1 (a :: TYPE rep1) rep2 (b :: TYPE rep2). a -> b -> ()
foo bndr_a pat@(myId -> bndr_b) = ()
