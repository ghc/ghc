-- Test for http://ghc.haskell.org/trac/ghc/ticket/2533
import System.Environment
import Data.List
main = do
 (n:_) <- getArgs
 print (genericTake (read n) "none taken")
 print (genericDrop (read n) "none dropped")
 print (genericSplitAt (read n) "none split")
