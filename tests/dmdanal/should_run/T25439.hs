{-# LANGUAGE MagicHash, UnboxedTuples, BlockArguments #-}

import Prelude hiding (break)
import GHC.Exts (PromptTag#, newPromptTag#, prompt#, control0#)
import GHC.IO (IO(..), unIO)
import Control.Monad (forever)

main :: IO ()
main = do
  putStrLn "before"
  broken >>= putStrLn
  putStrLn "after"

broken :: IO String
broken = do
  loop \l -> do
    break l "broken"

{-# NOINLINE loop #-}
loop :: (PromptTag# a -> IO ()) -> IO a
loop f = IO \rw0 -> case newPromptTag# rw0 of
  (# rw1, tag #) -> prompt# tag (unIO (forever (f tag))) rw1

break :: PromptTag# a -> a -> IO b
break tag x = IO (control0# tag \_ rw1 -> (# rw1, x #))
