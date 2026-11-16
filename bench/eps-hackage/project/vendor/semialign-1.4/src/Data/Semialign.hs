{-# LANGUAGE Trustworthy #-}
-- | Zipping and aligning of functors with non-uniform shapes.
--
--
module Data.Semialign (
    -- * Classes
    Semialign (..),
    Align (..),
    Unalign (..),
    Zip (..),
    Repeat (..),
    Unzip (..),
    unzipDefault,
    -- * Specialized aligns
    salign, padZip, padZipWith,
    lpadZip, lpadZipWith,
    rpadZip, rpadZipWith,
    alignVectorWith,
    ) where

import Data.Semialign.Internal
