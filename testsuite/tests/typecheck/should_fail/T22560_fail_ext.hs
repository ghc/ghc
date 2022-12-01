{-# LANGUAGE NoTypeAbstractions #-}

module T22560_fail_ext where

import Data.Kind

data T @k (a :: k) @(j :: Type) (b :: j)