{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module T15957 where

data P = P { x :: Int, y :: Int }

g1 P{..} = x + 3 -- x from .. is used
g2 P{x, ..} = x + y -- y from .. is used, even if it's in a weird style

old P{..} | x < 5 = 10

-- Record wildcards in lets have different scoping rules.. they bring
-- all the identifiers into scope
do_example :: IO Int
do_example = do
  let P{..} = P 1 2
  return $ x + y

let_in_example =
  let P{..} = P 1 2
  in x + 4
