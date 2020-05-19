{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Monad.Graded as Graded
import Vector as Graded


main = do
  print $ toList $([| Graded.do
    x <- VCons 1 (VCons 2 VNil)
    y <- VCons 1 (VCons 2 VNil)
    Graded.return (x, y) |])
  print $ toList $([| Graded.mdo
    z <- VCons (take 8 y) VNil
    y <- VCons (1 : zipWith (+) y (0 : y)) VNil
    Graded.return z |])
