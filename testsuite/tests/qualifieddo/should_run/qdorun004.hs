{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecursiveDo #-}
import qualified Control.Monad.Fix as P
import Prelude (print, ($))
import qualified Prelude as P

return :: a -> [a]
return x = [x, x]

-- Tests that QualifiedDo doesn't affect return
main = do
  print $ P.do
    x <- [1, 2]
    return x
  print $ P.mdo
    x <- [1, 2]
    return x
