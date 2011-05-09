{-# LANGUAGE Arrows, RebindableSyntax #-}
module T4851 where

import Prelude hiding ( id, (.) )

import Control.Category	( Category(..) )
import Control.Arrow

garbage x =
  proc b ->
    do rec (c, d) <- undefined -< (b, d)
       returnA -< c
