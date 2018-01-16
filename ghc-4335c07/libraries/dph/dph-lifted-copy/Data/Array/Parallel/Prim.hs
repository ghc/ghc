{-# OPTIONS_HADDOCK hide #-}

-- |This modules defines the interface between the DPH libraries and the compiler.  In particular,
-- it exports exactly those definitions that are used by either the desugarer (to remove parallel
-- array syntax) or by the vectoriser (to generate vectorised code).
--
-- The DPH libraries can evolve between compiler releases as long as this interface remains the
-- same.
--
-- WARNING: All modules in this package that need to be vectorised (i.e., are compiled with
--          '-fvectorise' must directly or indirectly import this module).  This is to ensure that
--          the build system does not attempt to compile a vectorised module before all definitions
--          that are required by the vectoriser are available.

-- #hide
module Data.Array.Parallel.Prim (
  PArray, PData, PDatas(..), PRepr, PA(..), PR(..),
  replicatePD, emptyPD, packByTagPD, combine2PD,
  Scalar(..),
  scalar_map, scalar_zipWith, scalar_zipWith3, scalar_zipWith4, scalar_zipWith5, scalar_zipWith6,
  scalar_zipWith7, scalar_zipWith8,
  Void, Sum2(..), Sum3(..), Wrap(..),
  void, fromVoid, pvoid, pvoids#, punit,
  (:->)(..), 
  closure, liftedClosure, ($:), liftedApply, closure1, closure2, closure3, closure4, 
  closure5, closure6, closure7, closure8,
  Sel2,  replicateSel2#, tagsSel2, elementsSel2_0#, elementsSel2_1#,
  Sels2, lengthSels2#,
  replicatePA_Int#, replicatePA_Double#,
  emptyPA_Int#, emptyPA_Double#,
  packByTagPA_Int#, packByTagPA_Double#,
  combine2PA_Int#, combine2PA_Double#,

  tup2, tup3, tup4, tup5
) where

-- We use explicit import lists here to make the vectoriser interface explicit and keep it under
-- tight control.
--
import Data.Array.Parallel.PArray.Base            (PArray)
import Data.Array.Parallel.PArray.Scalar          (Scalar(..))
import Data.Array.Parallel.PArray.ScalarInstances ( {-we require instances-} )
import Data.Array.Parallel.PArray.PRepr           (PRepr, PA(..), replicatePD, emptyPD, packByTagPD,
                                                   combine2PD)
import Data.Array.Parallel.PArray.Types           (Void, Sum2(..), Sum3(..), Wrap(..), void,
                                                   fromVoid)
import Data.Array.Parallel.PArray.PReprInstances  ( {-we required instances-} )
import Data.Array.Parallel.PArray.PData           (PData, PDatas, PR(..))
import Data.Array.Parallel.PArray.PDataInstances  (pvoid, punit, Sels2)
import Data.Array.Parallel.Lifted.Closure         ((:->)(..), closure, liftedClosure, ($:),
                                                   liftedApply, closure1, closure2, closure3, closure4, 
												   closure5, closure6, closure7, closure8)
import Data.Array.Parallel.Lifted.Unboxed         (Sel2, replicateSel2#, tagsSel2, elementsSel2_0#,
                                                   elementsSel2_1#,
                                                   replicatePA_Int#, replicatePA_Double#,
                                                   emptyPA_Int#, emptyPA_Double#,
                                                   {- packByTagPA_Int#, packByTagPA_Double# -}
                                                   combine2PA_Int#, combine2PA_Double#)
import Data.Array.Parallel.Lifted.Scalar          (scalar_map, scalar_zipWith, scalar_zipWith3, scalar_zipWith4,
                                                   scalar_zipWith5, scalar_zipWith6, scalar_zipWith7, scalar_zipWith8)
import Data.Array.Parallel.Prelude.Tuple          (tup2, tup3, tup4)
import GHC.Exts

packByTagPA_Int#, packByTagPA_Double# :: a
packByTagPA_Int#    = error "Data.Array.Parallel.Prim: 'packByTagPA_Int#' not implemented"
packByTagPA_Double# = error "Data.Array.Parallel.Prim: 'packByTagPA_Double#' not implemented"



-- Fake definitions involving PDatas.
-- The dph-lifted-copy backend doesn't used PDatas, but we need to define
-- this stuff as the vectoriser expects it to be here.
-- The vectoriser will generate instances of the PA dictionary involving
-- PDatas, but this backend will never call those methods.
pvoids#  :: Int# -> PDatas Void
pvoids#  = error "Data.Array.Parallel.Prim.voids: not used in this backend"

lengthSels2# :: Sels2 -> Int#
lengthSels2# _ = 0#


tup5    :: (PA a, PA b, PA c, PA d)
        =>  a :-> b :-> c :-> d :-> e :-> (a, b, c, d, e)
tup5    = error "Data.Array.Paralle.Prim.tup5: not used in this backend"


data instance PDatas (a, b, c)
        = PTuple3s (PDatas a) (PDatas b) (PDatas c)
        
data instance PDatas (a, b, c, d)
        = PTuple4s (PDatas a) (PDatas b) (PDatas c) (PDatas d)
        
data instance PDatas (a, b, c, d, e)
        = PTuple5s (PDatas a) (PDatas b) (PDatas c) (PDatas d) (PDatas e)

newtype instance PDatas (Wrap a)
        = PWraps (PDatas a)

        
