{-# LANGUAGE QualifiedDo #-}

import qualified Monad.Graded as Graded
import Vector as Graded


main = do
  putStrLn "The unqualified do still works."
  print $ toList $ Graded.do
    x <- VCons 1 (VCons 2 VNil)
    y <- VCons 1 (VCons 2 VNil)
    Graded.return (x, y)
  -- test Graded.fail
  print $ toList $ Graded.do
    1 <- VCons 1 VNil
    Graded.return 1
