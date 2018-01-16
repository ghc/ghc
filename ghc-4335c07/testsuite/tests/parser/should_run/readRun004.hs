{-# LANGUAGE UnboxedTuples, MagicHash #-}

-- should_run to make sure linking succeeds
-- (curried unboxed tuples with both boxed
--  and unboxed components).
-- See Trac #1509; also Note [Primop wrappers] in Id.lhs

import GHC.Exts

main = do
  case curried 9.0## 't'# of
    (# i#, u@(), d1#, c1#, f#, w#, d2#, c2# #)
       -> print ( I# i#, u, D# d1#, C# c1#, F# f#, W# w#, D# d2#, C# c2# )
  print $ map_ ((#,#) True) ['a','b','c']

-- try NOINLINE to make sure the currying isn't eliminated
-- too soon, but also test the other one without NOINLINE
-- for variety of testing
{-# NOINLINE curried #-}
curried :: Double# -> Char# ->
   (# Int#, (), Double#, Char#, Float#, Word#, Double#, Char# #)
curried = (#,,,,,,,#) 3# () 4.0## 'f'# 5.0# 32##

map_ :: (a -> (# b, c #)) -> [a] -> [(b,c)]
map_ f [] = []
map_ f (a:as) = case f a of
   (# b, c #) -> (b, c) : map_ f as

