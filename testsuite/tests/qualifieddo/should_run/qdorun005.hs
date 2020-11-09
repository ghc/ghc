{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RebindableSyntax #-}
import qualified Monad.Graded as Graded
import Vector
import Prelude (print, ($))
import qualified Prelude as P

xs >>= f = 'c' : P.concatMap f xs
(>>) = (P.>>)

main = do
  print $ toList $ Graded.do
    x <- VCons 'a' (VCons 'b' VNil)
    Graded.return x
  print $ do
    a <- ['a', 'b']
    P.return a
