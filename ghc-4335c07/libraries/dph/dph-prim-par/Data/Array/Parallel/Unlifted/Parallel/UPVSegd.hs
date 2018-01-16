{-# LANGUAGE CPP #-}
#include "fusion-phases.h"
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}

-- | Parallel virtual segment descriptors.
--
--   See "Data.Array.Parallel.Unlifted" for how this works.
--
module Data.Array.Parallel.Unlifted.Parallel.UPVSegd 
        ( -- * Types
          UPVSegd

          -- * Consistency check
        , valid

          -- * Constructors
        , mkUPVSegd
        , fromUPSegd
        , fromUPSSegd
        , empty
        , singleton
        , replicated
        
        -- * Predicates
        , isManifest
        , isContiguous

        -- * Projections
        , length
        , takeVSegids, takeVSegidsRedundant
        , takeUPSSegd, takeUPSSegdRedundant
        , takeDistributed
        , takeLengths
        , getSeg

        -- * Demotion
        , unsafeDemoteToUPSSegd
        , unsafeDemoteToUPSegd

        -- * Operators
        , updateVSegs
        , updateVSegsReachable

        , appendWith
        , combine2)
where
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Permute
import Data.Array.Parallel.Unlifted.Parallel.UPSel              (UPSel2)
import Data.Array.Parallel.Unlifted.Parallel.UPSSegd            (UPSSegd)
import Data.Array.Parallel.Unlifted.Parallel.UPSegd             (UPSegd)
import Data.Array.Parallel.Unlifted.Sequential.USegd            (USegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Vector)
import Data.Array.Parallel.Pretty                               hiding (empty)
import Prelude                                                  hiding (length)
import qualified Data.Array.Parallel.Unlifted.Distributed.Data.USegd  as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as US
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd as USSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSel    as UPSel
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd   as UPSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSSegd  as UPSSegd

here :: String -> String
here s = "Data.Array.Parallel.Unlifted.Parallel.UPVSegd." Prelude.++ s

-- UPVSegd ---------------------------------------------------------------------
-- | Parallel Virtual Segment descriptor.
--   
data UPVSegd 
        = UPVSegd 
        { upvsegd_manifest      :: !Bool
          -- ^ When the vsegids field holds a lazy (V.enumFromTo 0 (len - 1))
          --   then this field is True. This lets us perform some operations like
          --   demoteToUPSSegd without actually creating the vsegids field.
        
          -- | Virtual segment identifiers that indicate what physical segment
          --   to use for each virtual segment.
        , upvsegd_vsegids_redundant     :: Vector Int           -- LAZY FIELD 
        , upvsegd_vsegids_culled        :: Vector Int           -- LAZY FIELD
        
          -- | Scattered segment descriptor that defines how physical segments
          --   are layed out in memory.
        , upvsegd_upssegd_redundant     :: UPSSegd              -- LAZY FIELD
        , upvsegd_upssegd_culled        :: UPSSegd              -- LAZY FIELD

          -- | Segment descriptors distributed over the gang,
          --   along with logical segment id and element offsets.
          --   Note that segment ids here refer to a vsegid,
          --   not a physical or scattered segment id.
          --   See `splitSegdOfElemsD` for an example.
        , upvsegd_dsegd                 :: Dist ((USegd,Int),Int) -- LAZY FIELD
        
        -- IMPORTANT:
        -- When vsegids are transformed due to a segmented replication operation, 
        -- if some of the segment lengths were zero, then we will end up with 
        -- physical segments that are unreachable from the vsegids.
        -- 
        -- For some operations (like indexing) the fact that we have unreachable
        -- psegids doesn't matter, but for others (like segmented fold) it does.
        -- The problem is that we perform segmented fold by first folding all 
        -- the physical segments, then replicating the results according to the 
        -- vsegids. If no vsegids referenced a physical segment then we didn't 
        -- need to fold it.
        -- 
        -- When vsegids are updated the version that may have unreachable psegs
        -- is stored in the vsegids_redundant and upssegd_redundant. The _culled
        -- versions are then set to a SUSPENDED call to callOnVSegids. If no
        -- consumers every demand the culled version then we never need to compute
        -- it.
        -- 
        -- The vsegids_redundant field must also be lazy (no bang) because when it
        -- has the value (V.enumFromTo 0 (len - 1)) we want to avoid building the
        -- enumeration unless it's strictly demanded.
        }
        deriving (Show)


-- | Pretty print the physical representation of a `UVSegd`
instance PprPhysical UPVSegd where
 pprp (UPVSegd _ _ vsegids _ upssegd _)
  = vcat
  [ text "UPVSegd" $$ (nest 7 $ text "vsegids: " <+> (text $ show $ US.toList vsegids))
  , pprp upssegd ]


-- | O(1). Check the internal consistency of a virutal segmentation descriptor.
---
--   * TODO: this doesn't do any checks yet.
--
valid :: UPVSegd -> Bool
valid UPVSegd{} = True
{-# NOINLINE valid #-}
--  NOINLINE because it's only used during debugging anyway.


-- Constructors ---------------------------------------------------------------
-- NOTE: these are NOINLINE for now just so it's easier to read the core.
--       we can INLINE them later.

-- | O(1). Construct a new virtual segment descriptor.
mkUPVSegd
        :: Vector Int   -- ^ Array saying which physical segment to use for
                        --   each virtual segment.
        -> UPSSegd      -- ^ Scattered segment descriptor defining the physical
                        --   segments.
        -> UPVSegd

mkUPVSegd vsegids ussegd
        = UPVSegd False vsegids vsegids ussegd ussegd
        -- Lazy, so doesn't contribute to big-O
        $ mkDist vsegids ussegd
{-# INLINE_UP mkUPVSegd #-}

-- | O(segs). Distribute the logical over the gang.
mkDist
        :: Vector Int   -- ^ vsegids
        -> UPSSegd      -- ^ Scattered segment descriptor
        -> Dist ((USegd,Int),Int)
mkDist vsegids ussegd
        = USegd.splitSegdOnElemsD theGang
        $ USegd.fromLengths
        $ takeLengths' vsegids ussegd 

-- | O(segs). Promote a `UPSSegd` to a `UPVSegd`.
--   The result contains one virtual segment for every physical segment
--   defined by the `UPSSegd`.
---
--   * TODO: make this parallel, use parallel version of enumFromTo.
--
fromUPSSegd :: UPSSegd -> UPVSegd
fromUPSSegd upssegd
 = let  vsegids = US.enumFromTo 0 (UPSSegd.length upssegd - 1)
   in   UPVSegd True vsegids vsegids upssegd upssegd
        (mkDist vsegids upssegd)
{-# INLINE_UP fromUPSSegd #-}


-- | O(segs). Promote a `UPSegd` to a `UPVSegd`.
--   All segments are assumed to come from a flat array with sourceid 0.
--   The result contains one virtual segment for every physical segment
--   the provided `UPSegd`.
--
fromUPSegd :: UPSegd -> UPVSegd
fromUPSegd      = fromUPSSegd . UPSSegd.fromUPSegd
{-# INLINE_UP fromUPSegd #-}


-- | O(1). Construct an empty segment descriptor, with no elements or segments.
empty :: UPVSegd
empty
 = let  vsegids = US.empty
        upssegd = UPSSegd.empty
   in   UPVSegd True vsegids vsegids upssegd upssegd
        (mkDist vsegids upssegd)
{-# INLINE_UP empty #-}


-- | O(1). Construct a singleton segment descriptor.
--   The single segment covers the given number of elements in a flat array
--   with sourceid 0.
singleton :: Int -> UPVSegd
singleton n
 = let  vsegids = US.singleton 0
        upssegd = UPSSegd.singleton n
   in   UPVSegd True vsegids vsegids upssegd upssegd
        (mkDist vsegids upssegd)
{-# INLINE_UP singleton #-}


-- | O(1). Construct a `UPVSegd` that describes an array created by replicating
--   a single segment several times.
---
--   NOTE: This is a helpful target for rewrite rules, because when we 
--   see a 'replicated' we know that all segments in the virtual array
--   point to the same data.
replicated 
        :: Int          -- ^ Length of segment.
        -> Int          -- ^ Number of times replicated.
        -> UPVSegd

replicated len reps
 = let  -- We have a single physical segment.
        ssegd   = UPSSegd.singleton len

        -- All virtual segments point to the same physical segment.
   in   mkUPVSegd (US.replicate reps 0) ssegd                           -- TODO: use parallel replicate
{-# INLINE_U replicated #-}


-- Predicates -----------------------------------------------------------------
-- | O(1). Checks whether all the segments are manifest (unshared / non-virtual).
--   If this is the case, then the vsegids field will be [0..len-1]. 
--
--   Consumers can check this field, avoid demanding the vsegids field.
--   This can avoid the need for it to be constructed in the first place, due to
--   lazy evaluation.
--
isManifest :: UPVSegd -> Bool
isManifest      = upvsegd_manifest
{-# INLINE isManifest #-}


-- | O(1). True when the starts are identical to the usegd indices field and
--   the sources are all 0's. 
--
--   In this case all the data elements are in one contiguous flat
--   array, and consumers can avoid looking at the real starts and
--   sources fields.
--
isContiguous    :: UPVSegd -> Bool
isContiguous    = UPSSegd.isContiguous . upvsegd_upssegd_culled
{-# INLINE isContiguous #-}


-- Projections ----------------------------------------------------------------
-- INLINE trivial projections as they'll expand to a single record selector.

-- | O(1). Yield the overall number of segments.
length :: UPVSegd -> Int
length          = US.length . upvsegd_vsegids_redundant
{-# INLINE length #-}


-- | O(1). Yield the virtual segment ids of `UPVSegd`.
takeVSegids :: UPVSegd -> Vector Int
takeVSegids     = upvsegd_vsegids_culled
{-# INLINE takeVSegids #-}


-- | O(1). Take the vsegids of a `UPVSegd`, but don't require that every physical
--   segment is referenced by some virtual segment.
--
--   If you're just performing indexing and don't need the invariant that all
--   physical segments are reachable from some virtual segment, then use this
--   version as it's faster. This sidesteps the code that maintains the invariant.
--
--   The stated O(1) complexity assumes that the array has already been fully
--   evalauted. If this is not the case then we can avoid demanding the result
--   of a prior computation on the vsegids, thus reducing the cost attributed
--   to that prior computation.
takeVSegidsRedundant :: UPVSegd -> Vector Int
takeVSegidsRedundant = upvsegd_vsegids_redundant
{-# INLINE takeVSegidsRedundant #-}


-- | O(1). Yield the `UPSSegd` of `UPVSegd`.
takeUPSSegd :: UPVSegd -> UPSSegd
takeUPSSegd     = upvsegd_upssegd_culled
{-# INLINE takeUPSSegd #-}


-- | O(1). Take the `UPSSegd` of a `UPVSegd`, but don't require that every physical
--   segment is referenced by some virtual segment.
--
--   See the note in `takeVSegidsRedundant`.
takeUPSSegdRedundant :: UPVSegd -> UPSSegd
takeUPSSegdRedundant    = upvsegd_upssegd_redundant
{-# INLINE takeUPSSegdRedundant #-}


-- | O(1) or O(segs). Yield `USegd`s distributed over a logical view of this `UPVSegd`.
--   The complexity is only O(1) if this has already been evaluated.
takeDistributed :: UPVSegd -> Dist ((USegd,Int),Int)
takeDistributed = upvsegd_dsegd
{-# INLINE takeDistributed #-}


-- | O(segs). Yield the lengths of the segments described by a `UPVSegd`.
---
--   * TODO: This is slow and sequential.
--
takeLengths :: UPVSegd -> Vector Int
takeLengths (UPVSegd manifest _ vsegids _ upssegd _)
 | manifest     = UPSSegd.takeLengths upssegd
 | otherwise    
 = takeLengths' vsegids upssegd
{-# NOINLINE takeLengths #-}
--  NOINLINE because we don't want a case expression due to the test on the 
--  manifest flag to appear in the core program.

takeLengths'
        :: Vector Int   -- ^ Vsegids
        -> UPSSegd      -- ^ Scattered segment descriptor
        -> Vector Int
takeLengths' vsegids upssegd
 = let !lengths        = (UPSSegd.takeLengths upssegd)
   in  US.map (US.index (here "takeLengths") lengths) vsegids
{-# INLINE takeLengths' #-}


-- | O(1). Get the length, starting index, and source id of a segment.
---
--  NOTE: We don't return the segment index field from the `USSegd` as this refers
--        to the flat index relative to the `SSegd` array, rather than 
--        relative to the UVSegd array. If we tried to promote the `USSegd` index
--        to a `UVSegd` index it could overflow.
--
getSeg :: UPVSegd -> Int -> (Int, Int, Int)
getSeg upvsegd ix
 = let  vsegids = upvsegd_vsegids_redundant upvsegd
        upssegd = upvsegd_upssegd_redundant upvsegd
        (len, _index, start, source)
                = UPSSegd.getSeg upssegd (US.index (here "getSeg") vsegids ix)
   in   (len, start, source)
{-# INLINE_UP getSeg #-}


-- Demotion -------------------------------------------------------------------
-- | O(segs). Yield a `UPSSegd` that describes each segment of a `UPVSegd`
--   individually.
--
--   By doing this we lose information about which virtual segments
--   correspond to the same physical segments.
--
--   /WARNING/: Trying to take the `UPSegd` of a nested array that has been
--   constructed with replication can cause index space overflow. This is
--   because the virtual size of the corresponding flat data can be larger
--   than physical memory. If this happens then indices fields and 
--   element count in the result will be invalid.
-- 
unsafeDemoteToUPSSegd :: UPVSegd -> UPSSegd
unsafeDemoteToUPSSegd upvsegd
 | upvsegd_manifest upvsegd     = upvsegd_upssegd_culled upvsegd        -- TODO: take the redundant ones
 | otherwise
 = let  vsegids         = upvsegd_vsegids_culled upvsegd
        upssegd         = upvsegd_upssegd_culled upvsegd
        starts'         = bpermuteUP (UPSSegd.takeStarts  upssegd) vsegids
        sources'        = bpermuteUP (UPSSegd.takeSources upssegd) vsegids
        lengths'        = bpermuteUP (UPSSegd.takeLengths upssegd) vsegids
        upsegd'         = UPSegd.fromLengths lengths'
   in   UPSSegd.mkUPSSegd starts' sources' upsegd'
{-# NOINLINE unsafeDemoteToUPSSegd #-}
--  NOINLINE because it's complicated and won't fuse with anything.
--  In core we want to see when VSegds are being demoted.


-- | O(segs). Yield a `UPSegd` that describes each segment of a `UPVSegd`
--   individually, assuming all segments have been concatenated to 
--   remove scattering.
--
--   * See the warning in `unsafeDemoteToUPSSegd`.
---
--   * TODO: if the upvsegd is manifest and contiguous this can be O(1).
--
unsafeDemoteToUPSegd :: UPVSegd -> UPSegd
unsafeDemoteToUPSegd (UPVSegd _ _ vsegids _ upssegd _)
        = {-# SCC "unsafeDemoteToUPSegd" #-}
          UPSegd.fromLengths
        $ bpermuteUP (UPSSegd.takeLengths upssegd) vsegids
{-# NOINLINE unsafeDemoteToUPSegd #-}
--  NOINLINE because it's complicated and won't fuse with anything.
--  In core we want to see when VSegds are being demoted.


-- Operators ------------------------------------------------------------------
-- | Update the vsegids of a `UPVSegd`, and then cull the physical
--   segment descriptor so that all physical segments are reachable from
--   some virtual segment.
--
--   This function lets you perform filtering operations on the virtual segments,
--   while maintaining the invariant that all physical segments are referenced
--   by some virtual segment.
---
--   * TODO: make this parallel.
--     It runs the sequential 'cull' then reconstructs the UPSSegd.
-- 
updateVSegs :: (Vector Int -> Vector Int) -> UPVSegd -> UPVSegd
updateVSegs fUpdate (UPVSegd _ vsegids _ upssegd _ _)
 = let  
        -- When we transform the vsegids, we don't know whether they all 
        -- made it into the result. 
        vsegids_redundant      = fUpdate vsegids
 
        -- Cull the psegs down to just those reachable from the vsegids, 
        -- but do it lazilly so consumers can avoid demanding this 
        -- culled version and save creating it.
        (  vsegids_culled
         , ussegd_culled)       = USSegd.cullOnVSegids vsegids_redundant
                                $ UPSSegd.takeUSSegd upssegd

        upssegd_culled          = UPSSegd.fromUSSegd ussegd_culled

   in   UPVSegd False
                vsegids_redundant vsegids_culled
                upssegd           upssegd_culled
                (mkDist vsegids_redundant upssegd)
{-# NOINLINE updateVSegs #-}
--  NOINLINE because we want to see this happening in core.


-- | Update the vsegids  of `UPVSegd`, where the result is guaranteed to
--   cover all physical segments.
--
--   Using this version saves performing the 'cull' operation which 
--   discards unreachable physical segments.
--
--   * The resulting vsegids must cover all physical segments.
--     If they do not then there will be physical segments that are not 
--     reachable from some virtual segment, and subsequent operations
--     like segmented fold will have the wrong work complexity.
--
updateVSegsReachable :: (Vector Int -> Vector Int) -> UPVSegd -> UPVSegd
updateVSegsReachable fUpdate (UPVSegd _ _ vsegids _ upssegd _)
 = let  vsegids' = fUpdate vsegids
   in   UPVSegd False vsegids' vsegids' upssegd upssegd
        (mkDist vsegids' upssegd)
{-# NOINLINE updateVSegsReachable #-}
--  NOINLINE because we want to see this happening in core.


-- Append ---------------------------------------------------------------------
-- | Produce a segment descriptor that describes the result of appending two arrays.
--- 
--   * TODO: make this parallel.
--
appendWith
        :: UPVSegd      -- ^ Descriptor of first array.
        -> Int          -- ^ Number of flat physical arrays for first descriptor.
        -> UPVSegd      -- ^ Descriptor of second array.
        -> Int          -- ^ Number of flat physical arrays for second descriptor.
        -> UPVSegd

appendWith
        (UPVSegd _ _ vsegids1 _ upssegd1 _) pdatas1
        (UPVSegd _ _ vsegids2 _ upssegd2 _) pdatas2

 = let  -- vsegids releative to appended psegs
        vsegids1' = vsegids1
        vsegids2' = US.map (+ UPSSegd.length upssegd1) vsegids2
        
        -- append the vsegids
        vsegids'  = vsegids1' US.++ vsegids2'

        -- All data from the source arrays goes into the result
        upssegd'  = UPSSegd.appendWith
                                upssegd1 pdatas1
                                upssegd2 pdatas2
                                 
   in   UPVSegd False vsegids' vsegids' upssegd' upssegd'
        (mkDist vsegids' upssegd')
{-# NOINLINE appendWith #-}
--  NOINLINE because it doesn't need to be specialised
--           and we're worried about code explosion.


-- Combine --------------------------------------------------------------------
-- | Combine two virtual segment descriptors.
---
--   * TODO: make this parallel. 
--
combine2
        :: UPSel2       -- ^ Selector for the combine operation.
        -> UPVSegd      -- ^ Descriptor of first array.
        -> Int          -- ^ Number of flat physical arrays for first descriptor.
        -> UPVSegd      -- ^ Descriptor of second array.
        -> Int          -- ^ Number of flat physical arrays for second descriptor.
        -> UPVSegd
        
combine2
        upsel2
        (UPVSegd _ _ vsegids1 _ upssegd1 _) pdatas1
        (UPVSegd _ _ vsegids2 _ upssegd2 _) pdatas2

 = let  -- vsegids relative to combined psegs
        vsegids1' = vsegids1
        vsegids2' = US.map (+ (US.length vsegids1)) vsegids2

        -- combine the vsegids
        vsegids'  = US.combine2ByTag (UPSel.tagsUPSel2 upsel2)
                                    vsegids1' vsegids2'

         -- All data from the source arrays goes into the result
        upssegd'  = UPSSegd.appendWith
                                upssegd1 pdatas1
                                upssegd2 pdatas2
                                  
   in   UPVSegd False vsegids' vsegids' upssegd' upssegd'
        (mkDist vsegids' upssegd')
{-# NOINLINE combine2 #-}
--  NOINLINE because it doesn't need to be specialised
--           and we're worried about code explosion.

