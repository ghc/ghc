{-# LANGUAGE PatternSynonyms #-}
module T18247a where

import Control.Monad ()
import qualified Data.Sequence as Seq ()
import T18247b
    ( T,
      Nat(Z, S),
      Showable(..),
      Type,
      pattern ExNumPat,
      pattern Head,
      pattern Single,
      pattern Pair,
      pattern One,
      pattern Succ,
      pattern (:>),
      pattern (:<),
      pattern Empty,
      pattern Int,
      pattern Arrow,
      pattern P )
