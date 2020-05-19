{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecursiveDo #-}

import qualified Monad.Graded as Graded
import Vector as Graded


main = do
  print $ take 6 $ concat $ toList $ Graded.do
    rec
      VCons (take 6 y) VNil
      y <- VCons (1 : zipWith (+) y (0 : y)) VNil
    Graded.return y
