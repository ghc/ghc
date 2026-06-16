module T24136C (cVal) where

-- mkT is imported through T24136B's hs-boot interface, so here it is an unknown
-- closure referenced with tag 0.  Forcing it exercises the untagged-enter path
-- that must resolve through the static indirection kept in T24136B.
import {-# SOURCE #-} T24136B (mkT)

{-# OPAQUE cVal #-}
cVal :: Int
cVal = mkT `seq` 99
