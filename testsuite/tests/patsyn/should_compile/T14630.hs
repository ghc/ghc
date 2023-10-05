{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module T14630 where

pattern Tuple :: a -> b -> (a, b)
pattern Tuple{x, y} = (x, y)

{-# COMPLETE Tuple #-}

f :: (a, b) -> a
f Tuple{x} = x

g :: (Int, Int) -> Int
g Tuple{..} = x + y
