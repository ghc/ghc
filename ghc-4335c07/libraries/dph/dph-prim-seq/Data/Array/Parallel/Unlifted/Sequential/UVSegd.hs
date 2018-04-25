{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
#include "fusion-phases.h"

-- | Virtual Segment Descriptors.
--
--   See "Data.Array.Parallel.Unlifted" for how this works.
--
module Data.Array.Parallel.Unlifted.Sequential.UVSegd
        ( -- * Types
          UVSegd(..)

          -- * Consistency check
        , valid
        
          -- * Constructors
        , mkUVSegd
        , fromUSegd
        , fromUSSegd
        , empty
        , singleton
        , replicated
        
          -- * Predicates
        , isManifest
        , isContiguous
        
          -- * Projections
        , length
        , takeVSegids,  takeVSegidsRedundant
        , takeUSSegd,   takeUSSegdRedundant
        , takeLengths
        , getSeg

          -- * Operators
        , appendWith
        , combine2
        , updateVSegs
        , updateVSegsReachable
        , unsafeDemoteToUSSegd
        , unsafeDemoteToUSegd)
where
import Data.Array.Parallel.Unlifted.Sequential.USel
import Data.Array.Parallel.Unlifted.Sequential.USSegd           (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.USegd            (USegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Vector)
import Data.Array.Parallel.Pretty                               hiding (empty)
import Prelude                                                  hiding (length)
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as U
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd
import Debug.Trace

here :: String -> String 
here s = "Data.Array.Parallel.Unlifted.Sequential.UVSegd." ++ s


-- UVSegd ---------------------------------------------------------------------
-- | Virtual segment descriptor.
----   
--   * TODO: It would probably be better to represent the vsegids as a lens (function)
--           instead of a vector of segids. Much of the time the vsegids are just @[0..n]@
--
data UVSegd 
        = UVSegd 
        { uvsegd_manifest       :: !Bool
          -- ^ When the vsegids field holds a lazy @(U.enumFromTo 0 (len - 1))@
          --   then this field is True. This lets us perform some operations like
          --   `demoteToUPSSegd` without actually creating it.

          -- | Virtual segment identifiers that indicate what physical segment
          --   to use for each virtual segment.
        , uvsegd_vsegids_redundant     :: Vector Int           -- LAZY FIELD 
        , uvsegd_vsegids_culled        :: Vector Int           -- LAZY FIELD
        
          -- | Scattered segment descriptor that defines how physical segments
          --   are layed out in memory.
        , uvsegd_ussegd_redundant      :: USSegd               -- LAZY FIELD
        , uvsegd_ussegd_culled         :: USSegd               -- LAZY FIELD
        
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


instance PprPhysical UVSegd where
 pprp (UVSegd _ _ vsegids _ ussegd)
  = vcat
  [ text "UVSegd" $$ (nest 7 $ text "vsegids: " <+> (text $ show $ U.toList vsegids))
  , pprp ussegd ]



-- | O(1). Check the internal consistency of a virutal segmentation descriptor.
---
--   * TODO: check that all vsegs point to a valid pseg
valid :: UVSegd -> Bool
valid (UVSegd _ _ vsegids _ ussegd)
        = U.length vsegids == USSegd.length ussegd
{-# NOINLINE valid #-}
--  NOINLINE because it's only enabled during debugging anyway.


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct a new virtual segment descriptor.
--   All the provided arrays must have the same lengths.
mkUVSegd
        :: Vector Int   -- ^ (vsegids) Mapping from virtual to physical segments.
        -> USSegd       -- ^ Scattered Segment descriptor defining the 
                        --   physical segments.
        -> UVSegd

mkUVSegd vsegids ussegd
        = UVSegd False vsegids vsegids ussegd ussegd
{-# INLINE mkUVSegd #-}


-- | O(segs). Promote a plain `USegd` to a `UVSegd`.
--
--   The result contains one virtual segment for every physical segment
--   the provided `Segd`.
fromUSSegd :: USSegd -> UVSegd
fromUSSegd ussegd
 = let  vsegids = U.enumFromTo 0 (USSegd.length ussegd - 1)
   in   UVSegd True vsegids vsegids ussegd ussegd
{-# INLINE_U fromUSSegd #-}


-- | O(segs). Promote a plain `Segd` to a `VSegd`.
--
--   The result contains one virtual segment for every physical segment
--   the provided `SSegd`.
fromUSegd :: USegd -> UVSegd
fromUSegd
        = fromUSSegd . USSegd.fromUSegd
{-# INLINE_U fromUSegd #-}


-- | O(1). Construct an empty segment descriptor, with no elements or segments.
empty :: UVSegd
empty   
 = let  vsegids = U.empty
        ssegd   = USSegd.empty
   in   UVSegd True vsegids vsegids ssegd ssegd
{-# INLINE_U empty #-}


-- | O(1). Construct a singleton segment descriptor.
--   The single segment covers the given number of elements in a flat array
--   with sourceid 0.
singleton :: Int -> UVSegd
singleton n 
 = let  vsegids = U.singleton 0
        ssegd   = USSegd.singleton n
   in   UVSegd True vsegids vsegids ssegd ssegd
{-# INLINE_U singleton #-}


-- | O(1). Construct a `UVSegd` that describes an array created by replicating
--   a single segment several times.
---
--   NOTE: This is a helpful target for rewrite rules, because when we 
--   see a 'replicated' we know that all segments in the virtual array
--   point to the same data.
replicated 
        :: Int          -- ^ Length of segment.
        -> Int          -- ^ Number of times replicated.
        -> UVSegd

replicated len reps
 = let  -- We have a single physical segment.
        ssegd   = USSegd.singleton len

        -- All virtual segments point to the same physical segment.
   in   mkUVSegd (U.replicate reps 0) ssegd
{-# INLINE_U replicated #-}


-- Predicates -----------------------------------------------------------------
-- | O(1). Checks whether all the segments are manifest (unshared / non-virtual).
--   If this is the case, then the vsegids field will be [0..len-1]. 
--
--   Consumers can check this field, avoid demanding the vsegids field.
--   This can avoid the need for it to be generated in the first place, due to
--   lazy evaluation.
--
isManifest :: UVSegd -> Bool
isManifest      = uvsegd_manifest
{-# INLINE isManifest #-}


-- | O(1). Checks whether the starts are identical to the usegd indices field and
--   the sourceids are all 0's. 
--
--   In this case all the data elements are in one contiguous flat
--   array, and consumers can avoid looking at the real starts and
--   sources fields.
--
isContiguous :: UVSegd -> Bool
isContiguous    = USSegd.isContiguous . uvsegd_ussegd_culled
{-# INLINE isContiguous #-}


-- Projections ----------------------------------------------------------------
-- INLINE trivial projections as they'll expand to a single record selector.

-- | O(1). Yield the vsegids of a `UVSegd`
takeVSegids :: UVSegd -> Vector Int
takeVSegids     = uvsegd_vsegids_culled
{-# INLINE takeVSegids #-}


-- | O(1). Take the vsegids of a `UVSegd`, but don't require that every physical
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
takeVSegidsRedundant :: UVSegd -> Vector Int
takeVSegidsRedundant = uvsegd_vsegids_redundant
{-# INLINE takeVSegidsRedundant #-}


-- | O(1). Yield the `USSegd` of a `UVSegd`.
takeUSSegd :: UVSegd -> USSegd
takeUSSegd      = uvsegd_ussegd_culled
{-# INLINE takeUSSegd #-}


-- | O(1). Take the `UPSSegd` of a `UPVSegd`, but don't require that every physical
--   segment is referenced by some virtual segment.
--
--   See the note in `takeVSegidsRedundant`.
takeUSSegdRedundant :: UVSegd -> USSegd
takeUSSegdRedundant    = uvsegd_ussegd_redundant
{-# INLINE takeUSSegdRedundant #-}


-- | O(1). Yield the overall number of segments described by a `UVSegd`.
length :: UVSegd -> Int
length          = U.length . uvsegd_vsegids_redundant
{-# INLINE length #-}


-- | O(segs). Yield the lengths of the segments described by a `UVSegd`.
takeLengths :: UVSegd -> Vector Int
takeLengths (UVSegd manifest _ vsegids _ ussegd)
 | manifest     = USSegd.takeLengths ussegd 
 | otherwise
 = let 	!lengths	= USSegd.takeLengths ussegd
   in	U.map (U.index (here "takeLengths") lengths) vsegids
{-# NOINLINE takeLengths #-}
--  NOINLINE because we don't want a case expression due to the test on the 
--  manifest flag to appear in the core program.


-- | O(1). Get the length, starting index, and source id of a segment.

--  NOTE: We don't return the segment index field from the USSegd as this refers
--        to the flat index relative to the SSegd array, rather than 
--        relative to the UVSegd array. If we tried to promote the USSegd index
--        to a UVSegd index it could overflow.
--
getSeg :: UVSegd -> Int -> (Int, Int, Int)
getSeg uvsegd ix
 = let  vsegids = uvsegd_vsegids_redundant uvsegd
        ussegd  = uvsegd_ussegd_redundant  uvsegd
        (len, _index, start, source) 
                = USSegd.getSeg ussegd (U.index (here "getSeg") vsegids ix)
   in   (len, start, source)
{-# INLINE_U getSeg #-}


-- Demotion -------------------------------------------------------------------
-- | O(segs). Yield a `USSegd` that describes each segment of a `UVSegd` 
--   individually.
-- 
--   * By doing this we lose information about virtual segments corresponding
--     to the same physical segments.
-- 
--   * This operation is used in concatPR as the first step in eliminating
--     segmentation from a nested array.
-- 
unsafeDemoteToUSSegd :: UVSegd -> USSegd
unsafeDemoteToUSSegd uvsegd
 = traceEvent 
        (  "dph-prim-seq: UVSegd.unsafeDemoteToUSSSegd"
        ++ " length(segmap) = " ++ show (U.length $ takeVSegids uvsegd))
 $ if uvsegd_manifest uvsegd    
    then uvsegd_ussegd_culled uvsegd           -- TODO: take the redundant ones
    else let 
        vsegids         = uvsegd_vsegids_culled uvsegd
        ussegd          = uvsegd_ussegd_culled  uvsegd
        starts'         = U.bpermute (USSegd.takeStarts  ussegd) vsegids
        sources'        = U.bpermute (USSegd.takeSources ussegd) vsegids
        lengths'        = U.bpermute (USSegd.takeLengths ussegd) vsegids
        usegd'          = USegd.fromLengths lengths'
   in   USSegd.mkUSSegd starts' sources' usegd'
{-# NOINLINE unsafeDemoteToUSSegd #-}
--  NOINLINE because it's complicated and won't fuse with anything.


-- | O(segs). Yield a `USegd` that describes each segment of a `UVSegd`
--   individually, assuming all segments have been concatenated to 
--   remove scattering.
--
--   /WARNING/: Trying to take the `UPSegd` of a nested array that has been
--   constructed with replication can cause index space overflow. This is
--   because the virtual size of the corresponding flat data can be larger
--   than physical memory. If this happens then indices fields and 
--   element count in the result will be invalid.
-- 
--
unsafeDemoteToUSegd :: UVSegd -> USegd
unsafeDemoteToUSegd (UVSegd _ _ vsegids _ ussegd)
 = traceEvent
        (  "dph-prim-seq: UVSegd.unsafeDemoteToUSegd"
        ++ " length(segmap) = " ++ show (U.length vsegids))
 $ USegd.fromLengths
        $ U.bpermute (USSegd.takeLengths ussegd) vsegids
{-# NOINLINE unsafeDemoteToUSegd #-}
--  NOINLINE because it won't fuse with anything.



   
-- Operators ------------------------------------------------------------------
-- | Update the vsegids of `UPVSegd`, and then cull the physical
--   segment descriptor so that all phsyical segments are reachable from
--   some virtual segment.
--
--   This function lets you perform filtering operations on the virtual segments,
--   while maintaining the invariant that all physical segments are referenced
--   by some virtual segment.
-- 
updateVSegs :: (Vector Int -> Vector Int) -> UVSegd -> UVSegd
updateVSegs fUpdate (UVSegd _ _ vsegids _ ussegd)
 = let  -- When we transform the vsegids, we don't know whether they all 
        -- made it into the result. 
        vsegids_redundant      = fUpdate vsegids
 
        -- Cull the psegs down to just those reachable from the vsegids, 
        -- but do it lazilly so consumers can avoid demanding this 
        -- culled version and save creating it.
        (  vsegids_culled
         , ussegd_culled)       = USSegd.cullOnVSegids vsegids_redundant ussegd

   in   UVSegd False
               vsegids_redundant vsegids_culled
               ussegd            ussegd_culled
{-# INLINE_U updateVSegs #-}
--  INLINE_UP because we want to inline the parameter function fUpdate.


-- | Update the vsegids of `UPVSegd`, where the result covers
--   all physical segments.
--
--   * The resulting vsegids must cover all physical segments.
--     If they do not then there will be physical segments that are not 
--     reachable from some virtual segment, and performing operations like
--     segmented fold will waste work.
--
--   * Using this version saves performing the 'cull' operation which 
--     discards unreachable physical segments. This is O(result segments), 
--     but can be expensive in absolute terms.
--   
updateVSegsReachable :: (Vector Int -> Vector Int) -> UVSegd -> UVSegd
updateVSegsReachable fUpdate (UVSegd _ _ vsegids _ ssegd)
 = let  vsegids' = fUpdate vsegids
   in   UVSegd False vsegids' vsegids' ssegd ssegd
{-# INLINE_UP updateVSegsReachable #-}
--  INLINE_UP because we want to inline the parameter function fUpdate.



-- append ---------------------------------------------------------------------
-- | O(n)
--   Produce a segment descriptor describing the result of appending two arrays.

--   Note that the implementation of this is similar to `combine2UVSegd`
-- @
--  source1
--    VIRT1 [[0],[4,2],[5,6,7,8,9]]
--    PHYS1 UVSegd  vsegids:    [0,1,2]
--          USSegd  pseglens:   [1,2,5]
--                  psegstarts: [0,1,3]
--                  psegsrcs:   [0,0,0]
--          PData   PInt [0,4,2,5,6,7,8,9]
--
--  source2
--    VIRT2 [[1,2,3],[8,6,3],[9,3]]
--    PHYS2 UVSegd  vsegids:    [0,1,2]
--          USSegd  pseglens:   [3,3,2]
--                  psegstarts: [0,3,6]
--                  psegsrcs:   [0,0,0]
--          PData   PInt [1,2,3,8,6,3,9,3]
--
--   appended
--    VIRT  [[0],[4,2],[5,6,7,8,9],[1,2,3],[8,6,3],[9,3]]
--          UVSegd  vsegids:    [0,1,2,3,4,5]  -- shift second half
--          USSegd  pseglens:   [1,2,5,3,3,2]  -- appended
--                  psegstarts: [0,1,3,0,3,6]  -- appended
--                  psegsrcs:   [0,0,0,1,1,1]  -- shift second half
--          PData   PInt [0,4,2,5,6,7,8,9]     -- both pdatas in result
--                  PInt [1,2,3,8,6,3,9,3]     -- ...
-- @
-- 
appendWith
        :: UVSegd       -- ^ Descriptor of first array.
        -> Int          -- ^ Number of flat physical arrays for first descriptor.
        -> UVSegd       -- ^ Descriptor of second array.
        -> Int          -- ^ Number of flat physical arrays for second descriptor.
        -> UVSegd

appendWith
        (UVSegd _ _ vsegids1 _ ussegd1) pdatas1
        (UVSegd _ _ vsegids2 _ ussegd2) pdatas2

 = traceEvent 
        (  "dph-prim-seq: UVSegd.appendWith"
        ++ "length(result) = " ++ (show $ U.length vsegids1 + U.length vsegids2))
 $ let  
        -- vsegids releative to appended psegs
        vsegids1' = vsegids1
        vsegids2' = U.map (+ USSegd.length ussegd1) vsegids2
        
        -- append the vsegids
        vsegids'  = vsegids1' U.++ vsegids2'

        -- All data from the source arrays goes into the result
        ussegd'   = USSegd.appendWith
                                ussegd1 pdatas1
                                ussegd2 pdatas2
                                 
   in   UVSegd False vsegids' vsegids' ussegd' ussegd'
{-# NOINLINE appendWith #-}


-- combine --------------------------------------------------------------------
-- | O(n). Combine two virtual segment descriptors.


-- Note that the implementation of this is similar to `appendUVSegd`
-- @
-- source1
--    VIRT1 [[0],[4,2],[5,6,7,8,9]]
--    PHYS1 UVSegd  vsegids:    [0,1,2]
--          USSegd  pseglens:   [1,2,5]
--                  psegstarts: [0,1,3]
--                  psegsrcs:   [0,0,0]
--          PDATA   PInt [0,4,2,5,6,7,8,9]
--
-- source2
--    VIRT2 [[1,2,3],[8,6,3],[9,3]]
--    PHYS2 UVSegd  vsegids:    [0,1,2]
--          USSegd  pseglens:   [3,3,2]
--                  psegstarts: [0,3,6]
--                  psegsrcs:   [0,0,0]
--          PData   PInt [1,2,3,8,6,3,9,3]
--
--   combined with tags [1,0,0,1,0,1]
--    VIRT  [[1,2,3],[0],[4,2],[8,6,3],[5,6,7,8,9],[9,3]]
--    PHYS  VSSegd  vsegids:    [3,0,1,4,2,5] -- combine shifted vsegs
--          USSegd  pseglens:   [1,2,5,3,3,2] -- appended
--                  psegstarts: [0,1,3,0,3,6] -- appended
--                  psegsrcs:   [0,0,0,1,1,1] -- shift second half
--          PData   PInt [0,4,2,5,6,7,8,9]    -- both pdatas in result
--                  PInt [1,2,3,8,6,3,9,3]
-- @  
-- 
combine2
        :: USel2       -- ^ Selector for the combine operation.
        -> UVSegd      -- ^ Descriptor of first array.
        -> Int         -- ^ Number of flat physical arrays for first descriptor.
        -> UVSegd      -- ^ Descriptor of second array.
        -> Int         -- ^ Number of flat physical arrays for second descriptor.
        -> UVSegd
        
combine2  usel2
        (UVSegd _ _ vsegids1 _ ussegd1) pdatas1
        (UVSegd _ _ vsegids2 _ ussegd2) pdatas2

 = traceEvent
        (  "dph-prim-seq: UVSegd.combine2"
        ++ "length(result) = " ++ show (U.length $ tagsUSel2 usel2))
 $ let  
        -- vsegids relative to combined psegs
        vsegids1' = vsegids1
        vsegids2' = U.map (+ (U.length vsegids1)) vsegids2

        -- combine the vsegids
        vsegids'  = U.combine2ByTag (tagsUSel2 usel2)
                                    vsegids1' vsegids2'

         -- All data from the source arrays goes into the result
        ussegd'   = USSegd.appendWith
                                ussegd1 pdatas1
                                ussegd2 pdatas2
                                  
   in   UVSegd False vsegids' vsegids' ussegd' ussegd'
{-# NOINLINE combine2 #-}

