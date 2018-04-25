{-# LANGUAGE
    Arrows
  , TypeOperators
  #-}
-- This is extracted from arrow-list package
-- The problem was that some internal tuples that Arrows desugaring
-- generates the components in different orders depending on the order
-- of Uniques.
module A
(
 ifA
)
where

import Control.Arrow
import Prelude hiding (id, (.))

class Arrow arr => ArrowList arr where
  arrL :: (a -> [b]) -> a `arr` b
  mapL :: ([b] -> [c]) -> (a `arr` b) -> (a `arr` c)


empty :: ArrowList arr => (a `arr` b) -> a `arr` Bool
empty = mapL (\xs -> [if null xs then True else False])


ifA :: (ArrowList arr, ArrowChoice arr)
    => (a `arr` c)  -- ^ Arrow used as condition.
    -> (a `arr` b)  -- ^ Arrow to use when condition has results.
    -> (a `arr` b)  -- ^ Arrow to use when condition has no results.
    -> a `arr` b
ifA c t e = proc i -> do x <- empty c -< i; if x then e -< i else t -< i
