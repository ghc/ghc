{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QualifiedDo #-}

import qualified Monad.Graded as Graded
import Vector as Graded


main = do
  print $ toList $ Graded.do
    x <- VCons 1 (VCons 2 VNil)
    y <- VCons 1 (VCons 2 VNil)
    Graded.return (x, y)
  -- Test Graded.join
  print $ toList $ Graded.do
    x <- VCons 1 (VCons 2 VNil)
    y <- VCons 1 (VCons 2 VNil)
    VCons (y, x) VNil
