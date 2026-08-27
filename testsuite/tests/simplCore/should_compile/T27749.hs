module T27749 where

select :: Maybe Int -> (Int -> Int) -> Int
select m k = case m of { Nothing -> 0; Just x -> k x }
{-# INLINE select #-}

ordering :: Maybe Int -> Int -> Int
ordering m b = select m (\x -> x + b)
{-# OPAQUE ordering #-}
