module T25206 where

import T25206_helper ()

mod1 x = pf
 where
  (_,pf) = properFraction x
