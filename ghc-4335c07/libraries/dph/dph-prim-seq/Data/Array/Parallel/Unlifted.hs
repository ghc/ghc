{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
-- | Sequential implementation of the segmented array API defined in 
--   @dph-prim-interface@. 
--
--   There is a parallel implementation in @dph-prim-par@, 
--   so you probably want that instead.

--   The API is defined in @DPH_Header.h@ and @DPH_Interface.h@ to ensure that both
--   @dph-prim-par@ and @dph-prim-seq@ really do export the same symbols.
#include "DPH_Header.h"
import Data.Array.Parallel.Unlifted.Sequential.USel
import Data.Array.Parallel.Unlifted.Sequential
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd   as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd  as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.UVSegd  as UVSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector  as U
import qualified Data.Array.Parallel.Unlifted.Vectors            as US
#include "DPH_Interface.h"

-- NOTE:
-- See DPH_Interface.h for documentation. 
-- The defs should appear in the same order as they are listed in DPH_Interface.h


-- Basics ---------------------------------------------------------------------
class U.Unbox a => Elt a

-- | Arrays are stored as unboxed vectors. 
--   They have bulk-strict semantics, so demanding one element demands them all.
type Array                      = U.Vector


-- Constructors ---------------------------------------------------------------
empty                           = U.empty
(+:+)                           = (U.++)
append_s _ xd xs yd ys          = appendSU xd xs yd ys
append_vs _ xd xs yd ys         = appendSU xd' xs' yd' ys'
 where  xd' = unsafeDemoteToSegdOfVSegd xd
        yd' = unsafeDemoteToSegdOfVSegd yd
        xs' = extractsFromVectorsUVSegd xd xs
        ys' = extractsFromVectorsUVSegd yd ys

replicate                       = U.replicate
replicate_s                     = replicateSU
replicate_rs                    = replicateRSU
repeat n _                      = U.repeat n
indexed                         = U.indexed
indices_s                       = indicesSU
enumFromTo                      = U.enumFromTo
enumFromThenTo                  = U.enumFromThenTo
enumFromStepLen                 = U.enumFromStepLen
enumFromStepLenEach             = U.enumFromStepLenEach


-- Projections ----------------------------------------------------------------
length                          = U.length
index                           = U.index
indexs                          = indexsFromVector
indexs_avs                      = indexsFromVectorsUVSegd

extract                         = U.extract
extracts_nss                    = extractsFromNestedUSSegd
extracts_ass                    = extractsFromVectorsUSSegd
extracts_avs                    = extractsFromVectorsUVSegd
drop                            = U.drop


-- Update ---------------------------------------------------------------------
update                          = U.update


-- Permutation ----------------------------------------------------------------
permute                         = U.permute
bpermute                        = U.bpermute
mbpermute                       = U.mbpermute
bpermuteDft                     = U.bpermuteDft


-- Zipping and Unzipping ------------------------------------------------------
zip                             = U.zip
zip3                            = U.zip3
unzip                           = U.unzip
unzip3                          = U.unzip3
fsts                            = U.fsts
snds                            = U.snds


-- Map and ZipWith ------------------------------------------------------------
map                             = U.map
zipWith                         = U.zipWith


-- Scans and Folds ------------------------------------------------------------
scan                            = U.scan
fold                            = U.fold
fold_s                          = foldSU
fold_ss                         = foldSSU
fold_r                          = foldlRU
fold1                           = U.fold1
fold1_s                         = fold1SU
fold1_ss                        = fold1SSU
sum                             = U.sum
sum_r                           = sumRU
and                             = U.and


-- Packing and Filter ---------------------------------------------------------
pack                            = U.pack
filter                          = U.filter


-- Combine and Interleave -----------------------------------------------------
combine                         = U.combine
combine2 tags _                 = U.combine2ByTag tags
interleave                      = U.interleave


-- Selectors ------------------------------------------------------------------
type Sel2                       = USel2
mkSel2 tags idxs n0 n1 _        = mkUSel2 tags idxs n0 n1
tagsSel2                        = tagsUSel2
indicesSel2                     = indicesUSel2
elementsSel2_0                  = elementsUSel2_0
elementsSel2_1                  = elementsUSel2_1
repSel2 _                       = ()

type SelRep2                    = ()
mkSelRep2 _                     = ()
indicesSelRep2 tags _           = tagsToIndices2 tags
elementsSelRep2_0 tags _        = count tags 0
elementsSelRep2_1 tags _        = count tags 1


-- Segment Descriptors --------------------------------------------------------
type Segd                       = USegd.USegd
mkSegd                          = USegd.mkUSegd
validSegd                       = USegd.valid
emptySegd                       = USegd.empty
singletonSegd                   = USegd.singleton
lengthSegd                      = USegd.length
lengthsSegd                     = USegd.takeLengths
indicesSegd                     = USegd.takeIndices
elementsSegd                    = USegd.takeElements


-- Slice Segment Descriptors --------------------------------------------------
type SSegd                      = USSegd.USSegd
mkSSegd                         = USSegd.mkUSSegd
validSSegd                      = USSegd.valid
emptySSegd                      = USSegd.empty
singletonSSegd                  = USSegd.singleton
promoteSegdToSSegd              = USSegd.fromUSegd
isContiguousSSegd               = USSegd.isContiguous
lengthOfSSegd                   = USSegd.length
lengthsOfSSegd                  = USSegd.takeLengths
indicesOfSSegd                  = USSegd.takeIndices
startsOfSSegd                   = USSegd.takeStarts
sourcesOfSSegd                  = USSegd.takeSources
getSegOfSSegd                   = USSegd.getSeg
appendSSegd                     = USSegd.appendWith


-- Virtual Segment Descriptors ------------------------------------------------
type VSegd                      = UVSegd.UVSegd
mkVSegd                         = UVSegd.mkUVSegd
validVSegd                      = UVSegd.valid
emptyVSegd                      = UVSegd.empty
singletonVSegd                  = UVSegd.singleton
replicatedVSegd                 = UVSegd.replicated
promoteSegdToVSegd              = UVSegd.fromUSegd
promoteSSegdToVSegd             = UVSegd.fromUSSegd
isManifestVSegd                 = UVSegd.isManifest
isContiguousVSegd               = UVSegd.isContiguous
lengthOfVSegd                   = UVSegd.length
takeVSegidsOfVSegd              = UVSegd.takeVSegids
takeVSegidsRedundantOfVSegd     = UVSegd.takeVSegids
takeSSegdOfVSegd                = UVSegd.takeUSSegd
takeSSegdRedundantOfVSegd       = UVSegd.takeUSSegd
takeLengthsOfVSegd              = UVSegd.takeLengths
getSegOfVSegd                   = UVSegd.getSeg
unsafeDemoteToSSegdOfVSegd      = UVSegd.unsafeDemoteToUSSegd
unsafeDemoteToSegdOfVSegd       = UVSegd.unsafeDemoteToUSegd
updateVSegsOfVSegd              = UVSegd.updateVSegs
updateVSegsReachableOfVSegd     = UVSegd.updateVSegsReachable
appendVSegd                     = UVSegd.appendWith
combine2VSegd                   = UVSegd.combine2


-- Irregular 2D Arrays --------------------------------------------------------
class US.Unboxes a => Elts a
type Arrays                     = US.Vectors
emptys                          = US.empty
lengths                         = US.length
singletons                      = US.singleton
unsafeIndexs                    = US.unsafeIndex
unsafeIndex2s                   = US.unsafeIndex2
appends                         = US.append
fromVectors                     = US.fromVector
toVectors                       = US.toVector


-- Random Arrays --------------------------------------------------------------
randoms                         = U.random
randomRs                        = U.randomR


-- Array IO -------------------------------------------------------------------
class U.UIO a => IOElt a
hPut                            = U.hPut
hGet                            = U.hGet

toList                          = U.toList
fromList                        = U.fromList

