{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Patch (qc_prim) where

class PrimPatch (prim :: * -> * -> *)

class PrimPatch (PrimOf p) => PrimPatchBase p where
    type PrimOf (p :: * -> * -> *) :: * -> * -> *

type TestGenerator thing gen = (forall t ctx . (forall xx yy . thing xx yy -> t) -> (gen ctx -> t))

type family ModelOf (patch :: * -> * -> *) :: * -> *

data WithState s p x y = WithState {
                              _wsStartState :: s x
                            , _wsPatch      :: p x y
                            , _wsEndState   :: s y
                            }

arbitraryThing :: x -> TestGenerator thing (thing x)
arbitraryThing _ f p = f p

qc_prim :: forall prim x y .
           (PrimPatch prim
           , PrimOf prim ~ prim
           ) => prim x y -> [()]
qc_prim _ =
  concat
  [
   patch_repo_properties      (undefined :: prim x a)    "arbitrary"    arbitraryThing'
  ]
      where arbitraryThing' = arbitraryThing (undefined :: a)

patch_repo_properties :: p x y -> String -> TestGenerator (WithState (ModelOf (PrimOf p)) p) gen -> [()]
patch_repo_properties _ _genname _gen = undefined

