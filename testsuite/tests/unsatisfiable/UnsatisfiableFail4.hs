{-# LANGUAGE DataKinds, PartialTypeSignatures #-}

module UnsatisfiableFail4 where

import GHC.TypeError

data D = MkD

-- Check that we don't try to solve errors in kinds using Unsatisfiable.

instance Unsatisfiable (Text "msg") => Eq D where
  _ == _ = let y :: Maybe Maybe
               y = unsatisfiable
           in unsatisfiable

instance Unsatisfiable (Text "msg") => Ord D where
  compare _ _
    = let y :: _ => Maybe Maybe
          y = unsatisfiable
      in unsatisfiable
