{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -dcore-lint #-}

module T26350 where

import Control.Arrow (first)

infix 6 .-.

class AffineSpace p where
  type Diff p
  (.-.) :: p -> p -> Diff p

affineCombo :: (AffineSpace p, v ~ Diff p) => p -> (p,v) -> (v,v)
affineCombo z l = first (.-. z) l
