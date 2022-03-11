{-# LANGUAGE PolyKinds, UnliftedNewtypes, NoFieldSelectors #-}

module T20113 where

import GHC.Exts

newtype Y (a :: TYPE rep) = MkY { y_fld :: a }
