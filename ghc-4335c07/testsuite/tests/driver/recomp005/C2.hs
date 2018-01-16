{-# LANGUAGE RankNTypes #-}
module C (f, g) where

import A
import B

{-# RULES "f/g"  forall x . f (g x) = x #-}
