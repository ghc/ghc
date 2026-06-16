module T24136B (T(..), mkT) where

-- Importing T24136C closes an import cycle (T24136C imports this module through
-- its hs-boot interface), which forces mkT to be a genuine boot export.
import T24136C ()

data T = Tx Int | Ty

{-# OPAQUE mkT #-}
mkT :: T
mkT = Tx 7
