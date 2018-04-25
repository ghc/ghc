{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
#include "fusion-phases.h"

-- | Scattered Segment Descriptors.
--
--   See "Data.Array.Parallel.Unlifted" for how this works.
--
module Data.Array.Parallel.Unlifted.Sequential.USSegd 
        ( -- * Types
          USSegd(..)
        , valid

        -- * Constructors
        , mkUSSegd
        , empty
        , singleton
        , fromUSegd
        
        -- * Predicates
        , isContiguous
        
        -- * Projections
        , length
        , takeUSegd, takeLengths, takeIndices, takeElements
        , takeSources, takeStarts
        , getSeg
        
        -- * Operators
        , appendWith
        , cullOnVSegids)
where
import Data.Array.Parallel.Unlifted.Sequential.USegd            (USegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Vector)
import Data.Array.Parallel.Pretty                               hiding (empty)
import Prelude                                                  hiding (length)
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as U
import Debug.Trace

here :: String -> String 
here s = "Data.Array.Parallel.Unlifted.Sequential.USSegd." ++ s


-- USSegd ---------------------------------------------------------------------
-- | Scattered Segment Descriptor.
data USSegd
        = USSegd
        { ussegd_contiguous     :: !Bool
          -- ^ True when the starts are identical to the usegd indices field
          --        and the sources are all 0's. 
          --
          --   In this case all the data elements are in one contiguous flat
          --   array, and consumers can avoid looking at the real starts and
          --   sources fields.

        , ussegd_starts         :: Vector Int
          -- ^ Starting index of each segment in its flat array.
          -- 
          --   IMPORTANT: this field is lazy so we can avoid creating it when
          --              the flat array is contiguous.

        , ussegd_sources        :: Vector Int
          -- ^ Which flat array to take each segment from.
          -- 
          --   IMPORTANT: this field is lazy so we can avoid creating it when
          --              the flat array is contiguous.

        , ussegd_usegd          :: !USegd
          -- ^ Segment descriptor relative to a contiguous index space.
          --   This defines the length of each segment.
        }
        deriving (Show)


-- | Pretty print the physical representation of a `UVSegd`
instance PprPhysical USSegd where
 pprp (USSegd _ starts sources ssegd)
  = vcat
  [ text "USSegd" 
        $$ (nest 7 $ vcat
                [ text "starts:  " <+> (text $ show $ U.toList starts)
                , text "sources: " <+> (text $ show $ U.toList sources) ])
  , pprp ssegd ]


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct a new scattered segment descriptor.
--   All the provided arrays must have the same lengths.
mkUSSegd
        :: Vector Int   -- ^ Starting index of each segment in its flat array.
        -> Vector Int   -- ^ Which array to take each segment from.
        -> USegd        -- ^ Contiguous segment descriptor.
        -> USSegd

mkUSSegd = USSegd False
{-# INLINE mkUSSegd #-}


-- | O(1). Check the internal consistency of a scattered segment descriptor.
valid :: USSegd -> Bool
valid (USSegd _ starts srcids usegd)
        =  (U.length starts == USegd.length usegd)
        && (U.length srcids == USegd.length usegd)

{-# NOINLINE valid #-}
--  NOINLINE because it's only enabled during debugging anyway.


-- | O(1). Construct an empty segment descriptor, with no elements or segments.
empty :: USSegd
empty   = USSegd True U.empty U.empty USegd.empty
{-# INLINE_U empty #-}


-- | O(1). Construct a singleton segment descriptor.
--   The single segment covers the given number of elements in a flat array
--   with sourceid 0.
singleton :: Int -> USSegd
singleton n 
        = USSegd True (U.singleton 0) (U.singleton 0) (USegd.singleton n)
{-# INLINE_U singleton #-}


-- | O(segs). Promote a plain `USegd` to a `USSegd`.
--   All segments are assumed to come from a flat array with sourceid 0.
fromUSegd :: USegd -> USSegd
fromUSegd usegd
        = USSegd True 
                 (USegd.takeIndices usegd)
                 (U.replicate (USegd.length usegd) 0)
                 usegd
{-# INLINE_U fromUSegd #-}


-- Predicates -----------------------------------------------------------------
-- INLINE trivial projections as they'll expand to a single record selector.
-- | O(1). True when the starts are identical to the usegd indices field and
--   the sources are all 0's. 
--
--   In this case all the data elements are in one contiguous flat
--   array, and consumers can avoid looking at the real starts and
--   sources fields.
--
isContiguous :: USSegd -> Bool
isContiguous    = ussegd_contiguous
{-# INLINE isContiguous #-}


-- Projections ----------------------------------------------------------------
-- INLINE trivial projections as they'll expand to a single record selector.

-- | O(1). Yield the overall number of segments.
length :: USSegd -> Int
length          = USegd.length . ussegd_usegd 
{-# INLINE length #-}


-- | O(1). Yield the `USegd` of a `USSegd`.
takeUSegd   :: USSegd -> USegd
takeUSegd       = ussegd_usegd
{-# INLINE takeUSegd #-}


-- | O(1). Yield the lengths of the segments of a `USSegd`.
takeLengths :: USSegd -> Vector Int
takeLengths     = USegd.takeLengths . ussegd_usegd
{-# INLINE takeLengths #-}


-- | O(1). Yield the segment indices of a `USSegd`.
takeIndices :: USSegd -> Vector Int
takeIndices     = USegd.takeIndices . ussegd_usegd
{-# INLINE takeIndices #-}


-- | O(1). Yield the total number of elements covered by a `USSegd`.
takeElements :: USSegd -> Int
takeElements    = USegd.takeElements . ussegd_usegd
{-# INLINE takeElements #-}


-- | O(1). Yield the starting indices of a `USSegd`.
takeStarts :: USSegd -> Vector Int
takeStarts      = ussegd_starts
{-# INLINE takeStarts #-}


-- | O(1). Yield the source ids of a `USSegd`.
takeSources :: USSegd -> Vector Int
takeSources     = ussegd_sources
{-# INLINE takeSources #-}


-- | O(1). Get the length, segment index, starting index, and source id of a segment.
getSeg :: USSegd -> Int -> (Int, Int, Int, Int)
getSeg (USSegd _ starts sources usegd) ix
 = let  (len, ixl) = USegd.getSeg usegd ix
   in   ( len
        , ixl
        , U.index (here "getSeg") starts  ix
        , U.index (here "getSeg") sources ix)
{-# INLINE_U getSeg #-}


-- Operators ==================================================================

-- | O(n). Produce a segment descriptor that describes the result of appending
--   two arrays.
appendWith
        :: USSegd               -- ^ Segment descriptor of first nested array.
        -> Int                  -- ^ Number of flat data arrays used to represent first nested array.
        -> USSegd               -- ^ Segment descriptor of second nested array. 
        -> Int                  -- ^ Number of flat data arrays used to represent second nested array.
        -> USSegd
appendWith
        (USSegd _ starts1 srcs1 usegd1) pdatas1
        (USSegd _ starts2 srcs2 usegd2) _
 = traceEvent 
        (  "dph-prim-seq: USSegd.appendWith."
        ++ " length(result) = " ++ show (U.length starts1 + U.length starts2))
 $ USSegd False
        (starts1  U.++  starts2)
        (srcs1    U.++  U.map (+ pdatas1) srcs2)
        (USegd.append usegd1 usegd2)
{-# NOINLINE appendWith #-}
--  NOINLINE because we're worried about code explosion. Might be useful though.


-- | Cull the segments of a `USSegd` down to only those reachable from an array
--   of @vsegids@, and also update the @vsegids@ to point to the same segments
--   in the result.
--
cullOnVSegids :: Vector Int -> USSegd -> (Vector Int, USSegd)
cullOnVSegids vsegids (USSegd _ starts sources usegd)
 = {-# SCC "cullOnVSegids" #-}
   traceEvent 
        (  "dph-prim-seq: USSegd.cullOnVSegids."
        ++ " length(segmap) = " ++ show (U.length vsegids))
 $ let  -- Determine which of the psegs are still reachable from the vsegs.
        -- This produces an array of flags, 
        --    with reachable   psegs corresponding to 1
        --    and  unreachable psegs corresponding to 0
        -- 
        --  eg  vsegids:        [0 1 1 3 5 5 6 6]
        --   => psegids_used:   [1 1 0 1 0 1 1]
        --  
        --  Note that psegids '2' and '4' are not in vsegids_packed.
        !psegids_used
         = U.bpermuteDft (USegd.length usegd)
                         (const False)
                         (U.zip vsegids (U.replicate (U.length vsegids) True))

        -- Produce an array of used psegs.
        --  eg  psegids_used:   [1 1 0 1 0 1 1]
        --      psegids_packed: [0 1 3 5 6]
        !psegids_packed
         = U.pack (U.enumFromTo 0 (U.length psegids_used)) psegids_used

        -- Produce an array that maps psegids in the source array onto
        -- psegids in the result array. If a particular pseg isn't present
        -- in the result this maps onto -1.

        --  Note that if psegids_used has 0 in some position, then psegids_map
        --  has -1 in the same position, corresponding to an unused pseg.
         
        --  eg  psegids_packed: [0 1 3 5 6]
        --                      [0 1 2 3 4]
        --      psegids_map:    [0 1 -1 2 -1 3 4]
        !psegids_map
         = U.bpermuteDft (USegd.length usegd)
                         (const (-1))
                         (U.zip psegids_packed (U.enumFromTo 0 (U.length psegids_packed - 1)))

        -- Use the psegids_map to rewrite the packed vsegids to point to the 
        -- corresponding psegs in the result.
        -- 
        --  eg  vsegids:        [0 1 1 3 5 5 6 6]
        --      psegids_map:    [0 1 -1 2 -1 3 4]
        -- 
        --      vsegids':       [0 1 1 2 3 3 4 4]
        --
        !vsegids'  = U.map (U.index (here "cullOnVSegids") psegids_map) vsegids

        -- Rebuild the usegd.
        !starts'   = U.pack starts  psegids_used
        !sources'  = U.pack sources psegids_used

        !lengths'  = U.pack (USegd.takeLengths usegd) psegids_used
        !usegd'    = USegd.fromLengths lengths'
        
        !ussegd'   = USSegd False starts' sources' usegd'

     in (vsegids', ussegd')

{-# NOINLINE cullOnVSegids #-}
--  NOINLINE because it's complicated and won't fuse with anything
--  This can also be expensive and we want to see the SCC in profiling builds.


