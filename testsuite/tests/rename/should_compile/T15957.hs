{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module T15957 where

data P = P { x :: Int, y :: Int }

g1 P{..} = x + 3 -- x from .. is used
g2 P{x, ..} = x + y -- y from .. is used, even if it's in a weird style
