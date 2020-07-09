{-# LANGUAGE RankNTypes #-}

module T18412 where

hr :: (forall a. a -> a) -> ()
hr _ = ()

foo x = case x of () -> hr

-- This did not use to be allowed, because the
-- multiple branches have (the same) polytypes
-- Enhancement July 2020
bar x = case x of True -> hr
                  False -> hr
