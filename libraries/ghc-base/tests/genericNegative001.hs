-- Test for https://gitlab.haskell.org/ghc/ghc/issues/2533
import System.Environment
import qualified Data.List as L
main = do
 (n:_) <- getArgs
 print (L.genericTake (read n) "none taken")
 print (L.genericDrop (read n) "none dropped")
 print (L.genericSplitAt (read n) "none split")
