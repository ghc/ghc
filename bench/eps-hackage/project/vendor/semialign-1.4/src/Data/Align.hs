{-# LANGUAGE Trustworthy #-}
-- | 'These'-based aligning and unaligning of functors with non-uniform
-- shapes.
--
-- For a traversals traversal of (bi)foldable (bi)functors through said
-- functors see "Data.Crosswalk".
module Data.Align (
    Semialign (..),
    Align (..),
    Unalign (..),
    -- * Specialized aligns
    salign, padZip, padZipWith,
    lpadZip, lpadZipWith,
    rpadZip, rpadZipWith,
    alignVectorWith,
    ) where

import Data.Semialign.Internal
