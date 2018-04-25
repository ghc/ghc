{-# LANGUAGE TypeOperators, CPP #-}

-- | WARNING: 
--   This is an abstract interface. All the functions will just `error` if called. 
--
--   This module provides an API for the segmented array primitives used by DPH.
--   None of the functions here have implementations.
--
--   Client programs should use either the @dph-prim-seq@ or @dph-prim-par@
--   packages, as these provide the same API and contain real code.
--

--   NOTE: The API is enforced by the DPH_Header.h and DPH_Interface.h headers.
--   The dph-prim-interface, dph-prim-seq, and dph-prim-par modules all import
--   the same headers so we can be sure we're presenting the same API.

#include "DPH_Header.h"

import qualified Prelude as P
import Prelude ( Eq(..), Num(..), Bool(..), ($), (.) )

#include "DPH_Interface.h"


------------------------------------------------------------------------------
notImplemented :: P.String -> a
notImplemented fnName
 = P.error $ P.unlines
 [ "dph-prim-interface:Data.Array.Parallel.Unlifted." P.++ fnName
 , "This module is an abstract interface and does not contain real code."
 , "Use dph-prim-seq or dph-prim-par instead." ]
{-# NOINLINE notImplemented #-}


-- Types ----------------------------------------------------------------------
class Elt a
instance Elt a => Elt [a]
type Array a    = [a]


-- Constructors ---------------------------------------------------------------
empty                           = notImplemented "empty"
(+:+)                           = notImplemented "(+:+)"
append_s                        = notImplemented "append_s"
append_vs                       = notImplemented "append_vs"
replicate                       = notImplemented "replicate"
replicate_s                     = notImplemented "replicate_s"
replicate_rs                    = notImplemented "replicate_rs"
repeat                          = notImplemented "repeat"
indexed                         = notImplemented "indexed"
indices_s                       = notImplemented "indices_s"
enumFromTo                      = notImplemented "enumFromTo"
enumFromThenTo                  = notImplemented "enumFromThenTo"
enumFromStepLen                 = notImplemented "enumFromStepLen"
enumFromStepLenEach             = notImplemented "enumFromStepLenEach"


-- Conversions ----------------------------------------------------------------
nest                            = notImplemented "nest"
toList                          = notImplemented "toList"
fromList                        = notImplemented "fromList"
toList_s                        = notImplemented "toList_s"
fromList_s                      = notImplemented "fromList_s"


-- Projections ----------------------------------------------------------------
length                          = notImplemented "length"
index                           = notImplemented "index"
indexs                          = notImplemented "indexs"
indexs_avs                      = notImplemented "indexs_avs"
extract                         = notImplemented "extract"
extracts_nss                    = notImplemented "extract_nss"
extracts_ass                    = notImplemented "extract_ass"
extracts_avs                    = notImplemented "extract_avs"
drop                            = notImplemented "drop"


-- Update ---------------------------------------------------------------------
update                          = notImplemented "update"


-- Permutation ----------------------------------------------------------------
permute                         = notImplemented "permute"
bpermute                        = notImplemented "bpermute"
mbpermute                       = notImplemented "mbpermute"
bpermuteDft                     = notImplemented "bpermuteDft"


-- Zipping and Unzipping ------------------------------------------------------
zip                             = notImplemented "zip"
zip3                            = notImplemented "zip3"
unzip                           = notImplemented "unzip"
unzip3                          = notImplemented "unzip3"
fsts                            = notImplemented "fsts"
snds                            = notImplemented "snds"


-- Map and zipWith ------------------------------------------------------------
map                             = notImplemented "map"
zipWith                         = notImplemented "zipWith"


-- Scans and Folds -----------------------------------------------------------
scan                            = notImplemented "scan"
fold                            = notImplemented "fold"
fold_s                          = notImplemented "fold_s"
fold_ss                         = notImplemented "fold_ss"
fold_r                          = notImplemented "fold_r"
fold1                           = notImplemented "fold1"
fold1_s                         = notImplemented "fold1_s"
fold1_ss                        = notImplemented "fold1_ss"
sum                             = notImplemented "sum"
sum_r                           = notImplemented "sum_r"
and                             = notImplemented "and"


-- Packing and Combining ------------------------------------------------------
pack                            = notImplemented "pack"
filter                          = notImplemented "filter"
combine                         = notImplemented "combine"
combine2                        = notImplemented "combine2"
interleave                      = notImplemented "interleave"


-- Selectors ------------------------------------------------------------------
data Sel2 
        = Sel2 
        { sel2_tags      :: [Tag]
        , sel2_indices   :: [Int]
        , sel2_elements0 :: Int
        , sel2_elements1 :: Int }

type SelRep2    = ()

mkSel2                          = notImplemented "mkSel2"
tagsSel2                        = notImplemented "tagsSel2"
indicesSel2                     = notImplemented "indicesSel2"
elementsSel2_0                  = notImplemented "elementsSel2_0"
elementsSel2_1                  = notImplemented "elementsSel2_1"
repSel2                         = notImplemented "repSel2"

mkSelRep2                       = notImplemented "mkSelRep2"
indicesSelRep2                  = notImplemented "indicesSelRep2"
elementsSelRep2_0               = notImplemented "elementsSelRep2_0"
elementsSelRep2_1               = notImplemented "elementsSelRep2_1"


-- Segment Descriptors --------------------------------------------------------
data Segd 
        = Segd 
        { segd_lengths  :: [Int]
        , segd_indices  :: [Int]
        , segd_elements :: Int }

mkSegd                          = notImplemented "mkSegd"
emptySegd                       = notImplemented "emptySegd"
singletonSegd                   = notImplemented "singletonSegd"
validSegd                       = notImplemented "validSegd"
lengthSegd                      = notImplemented "lengthSegd"
lengthsSegd                     = notImplemented "lengthsSegd"
indicesSegd                     = notImplemented "indicesSegd"
elementsSegd                    = notImplemented "elementsSegd"


-- Scattered Segment Descriptors ----------------------------------------------
data SSegd
        = SSegd
        { ssegd_starts  :: [Int]
        , ssegd_sources :: [Int]
        , ssegd_segd    :: Segd }

mkSSegd                         = notImplemented "mkSSegd"
validSSegd                      = notImplemented "validSSegd"
emptySSegd                      = notImplemented "emptySSegd"
singletonSSegd                  = notImplemented "singletonSSegd"
promoteSegdToSSegd              = notImplemented "promoteSegdToSSegd"
isContiguousSSegd               = notImplemented "isContiguousSSegd"
lengthOfSSegd                   = notImplemented "lengthOfSSegd"
lengthsOfSSegd                  = notImplemented "lenghtsOfSSegd"
indicesOfSSegd                  = notImplemented "indicesOfSSegd"
startsOfSSegd                   = notImplemented "startsOfSSegd"
sourcesOfSSegd                  = notImplemented "sourcesOfSSegd"
getSegOfSSegd                   = notImplemented "getSegOfSSegd"
appendSSegd                     = notImplemented "appendSSegd"


-- Virtual Segment Descriptors ------------------------------------------------
data VSegd
        = VSegd
        { vsegd_vsegids :: [Int]
        , vsegd_ssegd   :: SSegd }

mkVSegd                         = notImplemented "mkVSegd"
validVSegd                      = notImplemented "validSSegd"       
emptyVSegd                      = notImplemented "emptyVSegd"
singletonVSegd                  = notImplemented "singletonVSegd"
replicatedVSegd                 = notImplemented "replicatedVSegd"
promoteSegdToVSegd              = notImplemented "promoteSegdToVSegd"
promoteSSegdToVSegd             = notImplemented "promoteSSegdToVSegd"
isContiguousVSegd               = notImplemented "isContiguousVSegd"
isManifestVSegd                 = notImplemented "isManifestVSegd"
lengthOfVSegd                   = notImplemented "lengthOfVSegd"
takeVSegidsOfVSegd              = notImplemented "takeVSegidsOfVSegd"
takeVSegidsRedundantOfVSegd     = notImplemented "takeVSegidsRedundantOfVSegd"
takeSSegdOfVSegd                = notImplemented "takeSSegdOfVSegd"
takeSSegdRedundantOfVSegd       = notImplemented "takeSSegdRedundantOfVSegd"
takeLengthsOfVSegd              = notImplemented "takeLengthsOfVSegd"
getSegOfVSegd                   = notImplemented "getSegOfVSegd"
unsafeDemoteToSSegdOfVSegd      = notImplemented "unsafeDemoteToSSegdOfVSegd"
unsafeDemoteToSegdOfVSegd       = notImplemented "unsafeDemoteToSegdOfVSegd"
updateVSegsOfVSegd              = notImplemented "updateVSegsOfVSegd"
updateVSegsReachableOfVSegd     = notImplemented "updateVSegsReachableOfVSegd"
appendVSegd                     = notImplemented "appendVSegd"
combine2VSegd                   = notImplemented "combine2VSegd"


-- Irregular two dimensional arrays -------------------------------------------
class Elts a
type Arrays a
        = [[a]]

emptys                          = notImplemented "emptys"
lengths                         = notImplemented "lengths"
singletons                      = notImplemented "singletons"
unsafeIndexs                    = notImplemented "unsafeIndexs"
unsafeIndex2s                   = notImplemented "unsafeIndex2s"
appends                         = notImplemented "appends"
fromVectors                     = notImplemented "fromVectors"
toVectors                       = notImplemented "toVectors"


-- Random Arrays --------------------------------------------------------------
randoms n                       = notImplemented "randoms"
randomRs n r                    = notImplemented "randomRs"


-- Array IO -------------------------------------------------------------------
class Elt a => IOElt a
hPut                            = notImplemented "hPut"
hGet                            = notImplemented "hGet"

