module T19474 where

import Control.Monad
import qualified Data.Vector.Storable.Mutable as M

main = do
  v <- M.new 1
  forM_ [0..10000] $ \some -> do
    addSome v 0 some
  M.unsafeRead v 0 >>= print

addSome :: M.IOVector Int -> Int -> Int -> IO ()
addSome m idx some = do
    val <- M.unsafeRead m idx
    M.unsafeWrite m  idx $ val + some
