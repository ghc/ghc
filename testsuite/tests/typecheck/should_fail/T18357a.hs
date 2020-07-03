{-# LANGUAGE PolyKinds, UnliftedNewtypes, StandaloneKindSignatures, TypeFamilies, GADTs, DataKinds #-}

module T18357a where

import Data.Kind
import GHC.Exts

newtype T :: TYPE r where
  MkT :: Int -> T

