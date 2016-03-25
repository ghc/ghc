{-# LANGUAGE TypeFamilies, KindSignatures, TypeInType #-}

module BadUnboxedTuple where

import GHC.Exts

type family F :: TYPE UnboxedTupleRep

foo :: F -> ()
foo _ = ()
