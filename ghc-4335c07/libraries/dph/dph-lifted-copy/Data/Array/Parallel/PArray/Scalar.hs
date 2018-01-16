{-# OPTIONS -fno-warn-orphans #-}
-- | Defines the class of scalar element types, as well as the 
--   PData instances for these types.
--
module Data.Array.Parallel.PArray.Scalar (
  Scalar(..),

  -- These functions have corresponding members in the PR class
  -- from Data.Array.Parallel.PArray.PData.
  emptyPRScalar,
  replicatePRScalar,
  replicatelPRScalar,
  repeatPRScalar, 
  indexPRScalar,
  extractPRScalar,
  bpermutePRScalar,
  appPRScalar,
  applPRScalar,
  packByTagPRScalar,
  combine2PRScalar,
  updatePRScalar,
  fromListPRScalar,
  nfPRScalar
)
where
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.Base
import Data.Array.Parallel.Base.DTrace
import qualified Data.Array.Parallel.Unlifted   as U
import GHC.Exts                                 (Int(..))


-- | Class of scalar types.
--   Scalar types are the ones that we can store in our underlying U.Arrays
--   (which are currently implemented as Data.Vectors).
--
--   To perform an operation on a PData array of scalar elements, we coerce
--   it to the underling U.Array and use the corresponding U.Array operators.
--
class U.Elt a => Scalar a where
  fromScalarPData :: PData a -> U.Array a
  toScalarPData   :: U.Array a -> PData a


-- Scalar Wrappers ------------------------------------------------------------
--  These wrappers work on (PData a) arrays when we know the element type 'a'
--  is scalar. For most of them we can just coerce the PData to the underling 
--  U.Array and use the corresponding U.Array operator.
--
--  The underlying U.Array may be processed in parallel or sequentially,
--  depending on what U.Array primitive library has been linked in.
--
emptyPRScalar :: Scalar a => T_emptyPR a
{-# INLINE emptyPRScalar #-}
emptyPRScalar 
  = toScalarPData U.empty

replicatePRScalar :: Scalar a => T_replicatePR a
{-# INLINE replicatePRScalar #-}
replicatePRScalar n# x
  = traceF "replicatePRScalar"
  $ toScalarPData (U.replicate (I# n#) x)

replicatelPRScalar :: Scalar a => T_replicatelPR a
{-# INLINE replicatelPRScalar #-}
replicatelPRScalar segd xs 
  = traceF "replicatelPRScalar"
  $ toScalarPData
  $ U.replicate_s segd 
  $ fromScalarPData xs

repeatPRScalar :: Scalar a => T_repeatPR a
{-# INLINE repeatPRScalar #-}
repeatPRScalar n# len# xs
  = traceF "repeatPRScalar"
  $ toScalarPData
  $ U.repeat (I# n#) (I# len#)
  $ fromScalarPData xs

indexPRScalar :: Scalar a => T_indexPR a
{-# INLINE indexPRScalar #-}
indexPRScalar xs i#
  = U.index "indexPRScalar" (fromScalarPData xs) (I# i#)

extractPRScalar :: Scalar a => T_extractPR a
{-# INLINE extractPRScalar #-}
extractPRScalar xs i# n#
  = traceF "extractPRScalar"
  $ toScalarPData
  $ U.extract (fromScalarPData xs) (I# i#) (I# n#)

bpermutePRScalar :: Scalar a => T_bpermutePR a
{-# INLINE bpermutePRScalar #-}
bpermutePRScalar xs _ is
  = traceF "bpermutePRScalar"
  $ toScalarPData
  $ U.bpermute (fromScalarPData xs) is

appPRScalar :: Scalar a => T_appPR a
{-# INLINE appPRScalar #-}
appPRScalar xs ys
  = traceF "appPRScalar"
  $ toScalarPData
  $ fromScalarPData xs U.+:+ fromScalarPData ys

applPRScalar :: Scalar a => T_applPR a
{-# INLINE applPRScalar #-}
applPRScalar segd xsegd xs ysegd ys
  = traceF "applPRScalar"
  $ toScalarPData
  $ U.append_s segd xsegd (fromScalarPData xs)
                    ysegd (fromScalarPData ys)
                        
packByTagPRScalar :: Scalar a => T_packByTagPR a
{-# INLINE packByTagPRScalar #-}
packByTagPRScalar xs _ tags t#
  = traceF "packByTagPRScalar"
  $ toScalarPData
  $ U.packByTag (fromScalarPData xs)
                tags
                (intToTag (I# t#))

combine2PRScalar :: Scalar a => T_combine2PR a
{-# INLINE combine2PRScalar #-}
combine2PRScalar _ sel xs ys 
  = traceF "combine2PRScalar"
  $ toScalarPData
  $ U.combine2 (U.tagsSel2 sel)
               (U.repSel2 sel)
               (fromScalarPData xs)
               (fromScalarPData ys)

updatePRScalar :: Scalar a => T_updatePR a
{-# INLINE updatePRScalar #-}
updatePRScalar xs is ys 
  = traceF "updatePRScalar"
  $ toScalarPData
  $ U.update (fromScalarPData xs)
             (U.zip is (fromScalarPData ys))

fromListPRScalar :: Scalar a => T_fromListPR a
{-# INLINE fromListPRScalar #-}
fromListPRScalar _ xs
  = toScalarPData (U.fromList xs)

nfPRScalar :: Scalar a => T_nfPR a
{-# INLINE nfPRScalar #-}
nfPRScalar xs
  = fromScalarPData xs `seq` ()
