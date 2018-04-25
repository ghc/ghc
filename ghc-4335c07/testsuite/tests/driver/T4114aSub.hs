module T4114aSub (assertKeep, assertNoKeep) where

import Control.Monad    (unless, when)
import System.Directory (doesFileExist)

assertNoKeep :: FilePath -> IO ()
assertNoKeep a =
   whenM (doesFileExist a) $
       error ("error: intermediate '" ++ a ++ "' exists")

assertKeep :: FilePath -> IO ()
assertKeep a =
   unlessM (doesFileExist a) $
       error ("error: intermediate '" ++ a ++ "' is missing")

whenM :: Monad m => m Bool -> m () -> m ()
whenM mp f = mp >>= \p -> when p f

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mp f = mp >>= \p -> unless p f
