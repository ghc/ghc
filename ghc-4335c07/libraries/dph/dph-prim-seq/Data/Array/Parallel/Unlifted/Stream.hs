
module Data.Array.Parallel.Unlifted.Stream
        ( -- * Segmented streams
          module Data.Array.Parallel.Unlifted.Stream.Segmented

          -- * Stream through segment descriptors
        , module Data.Array.Parallel.Unlifted.Stream.Ixs

          -- * Streams of scattered elements
        , module Data.Array.Parallel.Unlifted.Stream.Elems

          -- * Streams of scattered segments
        , module Data.Array.Parallel.Unlifted.Stream.Segments

          -- * Locked streams
        , module Data.Array.Parallel.Unlifted.Stream.Swallow
        , module Data.Array.Parallel.Unlifted.Stream.Locked)
where
import Data.Array.Parallel.Unlifted.Stream.Segmented
import Data.Array.Parallel.Unlifted.Stream.Ixs
import Data.Array.Parallel.Unlifted.Stream.Elems
import Data.Array.Parallel.Unlifted.Stream.Segments
import Data.Array.Parallel.Unlifted.Stream.Swallow
import Data.Array.Parallel.Unlifted.Stream.Locked
