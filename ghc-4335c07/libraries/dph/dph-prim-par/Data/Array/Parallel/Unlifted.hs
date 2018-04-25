{-# LANGUAGE CPP, NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
-- | Parallel implementation of the segmented array API defined in @dph-prim-interface@.
--
--   Some of them don't yet have parallel implementations, so we fall back
--   to the sequential ones from @dph-prim-seq@.
--
--   /WARNING:/ Although this library is intended to be used as a target
--   for the DPH vectoriser, it is also fine to use it directly from non
--   DPH programs. However, this library does not support nested parallelism
--   by itself. If you try to run further parallel computations in the workers
--   passed to `map`, `zipWith`, `fold` etc, then they will just run
--   sequentially.
---
--   This API is used by the @dph-lifted-*@ libraries, and is defined in
--   @DPH_Header.h@ and @DPH_Interface.h@. We use header files to ensure
--   that this API is implemented identically by both the 
--   @dph-prim-par@ and @dph-prim-seq@ packages.
--
#include "DPH_Header.h"
import Data.Array.Parallel.Base.TracePrim
import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Unlifted.Distributed ( DT )
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Unbox,   Vector)
import Data.Array.Parallel.Unlifted.Vectors                     (Unboxes, Vectors)
import Data.Array.Parallel.Unlifted.Parallel.UPSel
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd   as UPSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSSegd  as UPSSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPVSegd  as UPVSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import qualified Data.Array.Parallel.Unlifted.Vectors           as US
import qualified Data.Array.Parallel.Unlifted.Sequential        as Seq
import Prelude (($!))
#include "DPH_Interface.h"

-- NOTE 
-- See DPH_Interface.h for documentation. 
--
-- The defs should appear in the same order as they are listed in DPH_Interface.h
--
-- Operations with at least O(n) time will print trace messages to console when
-- dph-base/D/A/P/Config.tracePrimEnabled is set to True.
--

-- Basics ---------------------------------------------------------------------
class (Unbox a,   DT a) => Elt a

-- | Arrays are stored as unboxed vectors. 
--   They have bulk-strict semantics, so demanding one element demands them all.
type Array      = Vector


-- Constructors ---------------------------------------------------------------
empty   = Seq.empty

(+:+) arr1 arr2
        =  tracePrim (TraceAppend (Seq.length arr1 + Seq.length arr2))
        $! (Seq.++) arr1 arr2

append_s segd xd xs yd ys
 = let  arr     = appendSUP segd xd xs yd ys
   in   tracePrim (TraceAppend_s (Seq.length arr)) arr

append_vs segd xd xs yd ys
 = let  arr     = appendSUPV segd xd xs yd ys
   in   tracePrim (TraceAppend_vs (Seq.length arr)) arr

replicate n val 
        =  tracePrim (TraceReplicate n)
        $! replicateUP n val

replicate_s segd arr
        =  tracePrim (TraceReplicate_s (Seq.length arr))
        $! UPSegd.replicateWithP segd arr


replicate_rs n arr
        =  tracePrim (TraceReplicate_rs n (Seq.length arr))
        $! replicateRSUP n arr

repeat n _ arr
        =  tracePrim (TraceRepeat n (Seq.length arr))
        $! repeatUP n arr

indexed arr
        =  tracePrim (TraceIndexed (Seq.length arr))
        $! indexedUP arr

indices_s segd
 = let  arr     = UPSegd.indicesP segd
   in   tracePrim (TraceIndices_s (Seq.length arr)) arr

enumFromTo from to
 = let  arr     = enumFromToUP from to
   in   tracePrim (TraceEnumFromTo (Seq.length arr)) arr

enumFromThenTo from thn to
 = let  arr     = enumFromThenToUP from thn to
   in   tracePrim (TraceEnumFromThenTo (Seq.length arr)) arr
   
enumFromStepLen from step len
 = let  arr     = enumFromStepLenUP from step len
   in   tracePrim (TraceEnumFromStepLen (Seq.length arr)) arr

enumFromStepLenEach n starts steps lens
 = let  arr     = enumFromStepLenEachUP n starts steps lens
   in   tracePrim (TraceEnumFromStepLenEach (Seq.length arr)) arr


-- Projections ----------------------------------------------------------------
length          = Seq.length
index           = Seq.index
indexs          = indexsFromVector
indexs_avs      = indexsFromVectorsUPVSegdP

extract arr i n
        =  tracePrim (TraceExtract (Seq.length arr) i n)
        $! Seq.extract arr i n

extracts_nss    = extractsFromNestedUPSSegd
extracts_ass    = extractsFromVectorsUPSSegd
extracts_avs    = extractsFromVectorsUPVSegdP

drop n arr
        =  tracePrim (TraceDrop n (Seq.length arr))
        $! dropUP n arr


-- Update ---------------------------------------------------------------------
update arrSrc arrNew
        =  tracePrim (TraceUpdate (Seq.length arrSrc) (Seq.length arrNew))
        $! updateUP arrSrc arrNew


-- Permutation ----------------------------------------------------------------
permute arrSrc arrIxs
        =  tracePrim (TracePermute (Seq.length arrSrc))
        $! Seq.permute arrSrc arrIxs

bpermute arrSrc arrIxs
        =  tracePrim (TraceBPermute (Seq.length arrSrc))
        $! bpermuteUP arrSrc arrIxs


mbpermute f arrSrc streamIxs
        =  tracePrim (TraceMBPermute (Seq.length arrSrc))
        $! Seq.mbpermute f arrSrc streamIxs

bpermuteDft len f arrIxs
        =  tracePrim (TraceBPermuteDft len)
        $! Seq.bpermuteDft len f arrIxs


-- Zipping and Unzipping ------------------------------------------------------
zip     = Seq.zip
zip3    = Seq.zip3
unzip   = Seq.unzip
unzip3  = Seq.unzip3
fsts    = Seq.fsts
snds    = Seq.snds


-- Map and ZipWith ------------------------------------------------------------
map f arr
        =  tracePrim (TraceMap (Seq.length arr))
        $! mapUP f arr

zipWith f arr1 arr2
        =  tracePrim (TraceZipWith (Seq.length arr1) (Seq.length arr2))
        $! zipWithUP f arr1 arr2


-- Scans and Folds ------------------------------------------------------------
scan f x arr
        =  tracePrim (TraceScan (Seq.length arr))
        $! scanUP f x arr

fold f x arr
        =  tracePrim (TraceFold (Seq.length arr))
        $! foldUP f x arr

fold_s f x segd arr
        =  tracePrim (TraceFold_s (Seq.length arr))
        $! UPSegd.foldWithP f x segd arr

fold_ss = UPSSegd.foldWithP

fold_r f z segSize arr
        =  tracePrim (TraceFold_r (Seq.length arr))
        $! Seq.foldlRU f z segSize arr
        
fold1 f arr
        =  tracePrim (TraceFold1 (Seq.length arr))
        $! Seq.fold1 f arr

fold1_s f segd arr
        =  tracePrim (TraceFold1_s (Seq.length arr))
        $! UPSegd.fold1WithP f segd arr

fold1_ss = UPSSegd.fold1WithP

sum arr =  tracePrim (TraceSum (Seq.length arr))
        $! sumUP arr

sum_r x arr
        =  tracePrim (TraceSum_r (Seq.length arr))
        $! sumRUP x arr

and arr =  tracePrim (TraceAnd (Seq.length arr))
        $! andUP arr


-- Pack and Filter ------------------------------------------------------------
pack arrSrc arrFlag
        =  tracePrim (TracePack (Seq.length arrSrc))
        $! packUP arrSrc arrFlag

filter f src
 = let  dst     = filterUP f src
   in   tracePrim (TraceFilter (Seq.length src) (Seq.length dst)) dst


-- Combine and Interleave -----------------------------------------------------
combine arrSel arr1 arr2
        =  tracePrim (TraceCombine (Seq.length arrSel))
        $! combineUP arrSel arr1 arr2

combine2 arrTag sel arr1 arr2
        =  tracePrim (TraceCombine2 (Seq.length arrTag))
        $! combine2UP arrTag sel arr1 arr2

interleave arr1 arr2
        =  tracePrim (TraceInterleave (Seq.length arr1 + Seq.length arr2))
        $! interleaveUP arr1 arr2


-- Selectors ------------------------------------------------------------------
type Sel2               = UPSel2

mkSel2 tag is n0 n1 rep
        =  tracePrim (TraceMkSel2 (Seq.length is))
        $! mkUPSel2 tag is n0 n1 rep

tagsSel2 sel
 = let  tags    = tagsUPSel2 sel
   in   tracePrim (TraceTagsSel2 (Seq.length tags)) tags


indicesSel2 sel      
 = let  arr     = indicesUPSel2 sel
   in   tracePrim (TraceIndicesSel2 (Seq.length arr)) arr


elementsSel2_0                  = elementsUPSel2_0
elementsSel2_1                  = elementsUPSel2_1
repSel2                         = repUPSel2

type SelRep2                    = UPSelRep2
mkSelRep2                       = mkUPSelRep2

indicesSelRep2                  = indicesUPSelRep2
elementsSelRep2_0               = elementsUPSelRep2_0
elementsSelRep2_1               = elementsUPSelRep2_1


-- Segment Descriptors --------------------------------------------------------
type Segd                       = UPSegd.UPSegd
mkSegd                          = UPSegd.mkUPSegd
validSegd                       = UPSegd.valid
emptySegd                       = UPSegd.empty
singletonSegd                   = UPSegd.singleton
lengthSegd                      = UPSegd.length
lengthsSegd                     = UPSegd.takeLengths
indicesSegd                     = UPSegd.takeIndices
elementsSegd                    = UPSegd.takeElements


-- Scattered Segment Descriptors ----------------------------------------------
type SSegd                      = UPSSegd.UPSSegd
mkSSegd                         = UPSSegd.mkUPSSegd
promoteSegdToSSegd              = UPSSegd.fromUPSegd
validSSegd                      = UPSSegd.valid
emptySSegd                      = UPSSegd.empty
singletonSSegd                  = UPSSegd.singleton
isContiguousSSegd               = UPSSegd.isContiguous
lengthOfSSegd                   = UPSSegd.length
lengthsOfSSegd                  = UPSSegd.takeLengths
indicesOfSSegd                  = UPSSegd.takeIndices
startsOfSSegd                   = UPSSegd.takeStarts
sourcesOfSSegd                  = UPSSegd.takeSources
getSegOfSSegd                   = UPSSegd.getSeg
appendSSegd                     = UPSSegd.appendWith


-- Virtual Segment Descriptors ------------------------------------------------
type VSegd                      = UPVSegd.UPVSegd
mkVSegd                         = UPVSegd.mkUPVSegd
validVSegd                      = UPVSegd.valid
emptyVSegd                      = UPVSegd.empty
singletonVSegd                  = UPVSegd.singleton
replicatedVSegd                 = UPVSegd.replicated
promoteSegdToVSegd              = UPVSegd.fromUPSegd
promoteSSegdToVSegd             = UPVSegd.fromUPSSegd
isManifestVSegd                 = UPVSegd.isManifest
isContiguousVSegd               = UPVSegd.isContiguous
lengthOfVSegd                   = UPVSegd.length
takeVSegidsOfVSegd              = UPVSegd.takeVSegids
takeVSegidsRedundantOfVSegd     = UPVSegd.takeVSegidsRedundant
takeSSegdOfVSegd                = UPVSegd.takeUPSSegd
takeSSegdRedundantOfVSegd       = UPVSegd.takeUPSSegdRedundant
takeLengthsOfVSegd              = UPVSegd.takeLengths
getSegOfVSegd                   = UPVSegd.getSeg
unsafeDemoteToSSegdOfVSegd      = UPVSegd.unsafeDemoteToUPSSegd
unsafeDemoteToSegdOfVSegd       = UPVSegd.unsafeDemoteToUPSegd
updateVSegsOfVSegd              = UPVSegd.updateVSegs
updateVSegsReachableOfVSegd     = UPVSegd.updateVSegsReachable
appendVSegd                     = UPVSegd.appendWith
combine2VSegd                   = UPVSegd.combine2


-- Irregular 2D arrays --------------------------------------------------------
class (Unboxes a, DT a) => Elts a

type Arrays                     = Vectors
emptys                          = US.empty
lengths                         = US.length
singletons                      = US.singleton
unsafeIndexs                    = US.unsafeIndex
unsafeIndex2s                   = US.unsafeIndex2
appends                         = US.append
fromVectors                     = US.fromVector
toVectors                       = US.toVector


-- Random arrays --------------------------------------------------------------
randoms                         = Seq.random
randomRs                        = Seq.randomR


-- IO -------------------------------------------------------------------------
class Seq.UIO a => IOElt a
hPut                            = Seq.hPut
hGet                            = Seq.hGet
toList                          = Seq.toList
fromList                        = Seq.fromList
