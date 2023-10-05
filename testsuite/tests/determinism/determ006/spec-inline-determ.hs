module Roman where

-- This is a simplified version of simplCore/should_compile/spec-inline.hs
--
-- It reproduces a problem where workers get specialized in different ways
-- depending on the values of uniques.
--
-- Compare:
--
--   $s$wgo_s1CN :: Int# -> Int -> Int#
--   [LclId, Arity=2, Str=<L,U><L,U>]
--   $s$wgo_s1CN =
--     \ (sc_s1CI :: Int#) (sc_s1CJ :: Int) ->
--       case tagToEnum# @ Bool (<=# sc_s1CI 0#) of _ [Occ=Dead] {
--         False ->
--           $wgo_s1BU (Just @ Int (I# (-# sc_s1CI 1#))) (Just @ Int sc_s1CJ);
--         True -> 0#
--       }
--
-- vs
--
--   $s$wgo_s18mTj :: Int -> Int# -> Int#
--   [LclId, Arity=2, Str=<L,U><L,U>]
--   $s$wgo_s18mTj =
--     \ (sc_s18mTn :: Int) (sc_s18mTo :: Int#) ->
--       case tagToEnum# @ Bool (<=# sc_s18mTo 0#) of _ [Occ=Dead] {
--         False ->
--           $wgo_s18mUc
--             (Just @ Int (I# (-# sc_s18mTo 1#))) (Just @ Int sc_s18mTn);
--         True -> 0#
--       }

foo :: Int -> Int
foo n =
  go (Just n) (Just (6::Int))
  where
  go Nothing (Just x) = go (Just 10) (Just x)
  go (Just n) (Just x)
    | n <= 0    = 0
    | otherwise = go (Just (n-1)) (Just x)
