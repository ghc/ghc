{-# LANGUAGE PatternSynonyms #-}

module T25892 (D(T25892.K)) where

import T25892_aux (D)
import qualified T25892_aux as Internals ( D(..) )

pattern K :: D
pattern K = Internals.K
