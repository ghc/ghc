{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module T24686 where

import GHC.Exts
import GHC.Stack

{-
  on GHC 9.4 / 9.6 / 9.8 this panics with
  <no location info>: error:
    panic! (the 'impossible' happened)
  GHC version 9.4.8:
	typeKind
  forall {r :: RuntimeRep} (a :: TYPE r). a
  [r_aNu, a_aNy]
  a_aNy :: TYPE r_aNu
  Call stack:
      CallStack (from HasCallStack):
        callStackDoc, called at compiler/GHC/Utils/Panic.hs:182:37 in ghc:GHC.Utils.Panic
        pprPanic, called at compiler/GHC/Core/Type.hs:3059:18 in ghc:GHC.Core.Type

  This regression test exists to make sure the fix introduced between 9.8 and 9.11 does not get removed
  again.
-}

pattern Bug :: forall. HasCallStack => forall {r :: RuntimeRep} (a :: TYPE r). a
pattern Bug <- (undefined -> _unused)
  where
    Bug = undefined
