module T24294 where

import qualified Data.List

-- | 'unfoldr' needs to link to local definition and not to 'Data.List.unfoldr'
unfoldr = undefined
