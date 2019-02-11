{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module T15957_Fail where

data P = P { x :: Int, y :: Int }

f1 P{..} = 1 + 3 -- nothing bound is used
f2 P{x, ..} = x + 3 -- y bound but not used
f3 P{x, y, ..} = x + y -- no bindings left, i.e. no new useful bindings introduced

g2 P{x=a, ..} = a + 3
g3 P{x=a, y=b, ..} = a + b
g4 P{x=0, y=0,..} = 0
g4 _ = 0

-- Record wildcards in lets have different scoping rules.. they bring
-- all the identifiers into scope
do_example :: IO Int
do_example = do
  let P{..} = P 1 2
  return $ 0

let_in_example :: Int
let_in_example =
  let P{..} = P 1 2
  in 0

data Q = Q { a, b :: P }

nested :: Q -> Int
nested Q { a = P{..}, .. } = (case b of (P x1 _) -> x1)

