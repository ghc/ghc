-- | Common config and debugging functions. Imported by most modules.
module Data.Array.Parallel.Base 
        ( -- * Debugging infrastructure
          module Data.Array.Parallel.Base.Config
        , module Data.Array.Parallel.Base.Debug

          -- * Data constructor rags
        , module Data.Array.Parallel.Base.Tag

          -- * ST monad re-exported from GHC
        , ST(..)
        , runST)
where
import Data.Array.Parallel.Base.Debug
import Data.Array.Parallel.Base.Config
import Data.Array.Parallel.Base.Tag
import GHC.ST (ST(..), runST)

