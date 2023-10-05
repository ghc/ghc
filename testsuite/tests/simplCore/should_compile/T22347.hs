{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
module M where

import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

bfs :: Array Int [Int] -> ST s (STArray s Int ())
bfs g = do
    vis :: STArray s Int () <- newArray (bounds g) ()
    ch :: STArray s Int () <- newArray (bounds g) ()
    let go [] = pure () :: ST s ()
        go q = do
            flip mapM_ q $ \u -> do
                readArray vis (head (g!u))
                readArray ch u
                writeArray ch u ()
            go []
    go []
    pure ch
