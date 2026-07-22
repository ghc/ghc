{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Bifunctor (first)
import Data.Coerce
import Data.Function (on)
import Data.List (nubBy, sortOn, stripPrefix)
import GHC.Data.StringTrie as StringTrie
import MiniQuickCheck

main :: IO ()
main = do
  runTestsMain (Iterations 1000) tests
  -- runTests (Iterations 100) 9856592474076851907 tests

tests :: Test
tests = Group "StringTrie"
    [ Property "lookup_after_insert" prop_lookup_after_insert
    , Property "fromList == foldl (flip $ uncurry insert) empty" prop_fromList_foldl
    ]

type BString4 = BoundedList 4 (BoundedChar 4)

-- | @lookup qs (insert k v t)@ returns the values of matching keys ordered by
-- suffix length.
prop_lookup_after_insert
     -- | query string as a bounded list
  :: BoundedList 6 (BoundedChar 3)
     -- | length of the key to insert in @t@
  -> BoundedBy Int 3
     -- | value to insert in @t@
  -> BoundedBy Int 9
     -- | list of key-value pairs to build the trie @t@
  -> BoundedList 6 (BString4, Int `BoundedBy` 9)
  -> PropertyCheck
prop_lookup_after_insert bqs i0 v0 bkvs =
  let kvs = coerce bkvs :: [(String, Int)]
      t = fromList kvs
      qs = coerce @_ @String bqs
      i = min (abs (getBoundedBy i0)) (length qs)
      k = take i qs
      v = getBoundedBy v0
      -- (v1, sfx1) pairs where (k1, v1) is in t and k1 is a prefix of qs
      expected =
        [ (v1, sfx1)
        | kv@(k1, v1) <- nubBy ((==) `on` fst) $ (k, v) : reverse kvs
        , Just sfx1 <- [stripPrefix k1 qs]
        ]
   in
      propertyEqWithContext
        (unlines
            [ "k = " ++ show k
            , "v = " ++ show v
            , "qs = " ++ show qs
            , "kvs = " ++ show kvs
            ]
        )
        (StringTrie.lookup qs (insert k v t))
        (sortOn (length . snd) expected)

-- | @fromList xs == foldl (flip $ uncurry insert) empty xs@
prop_fromList_foldl
     -- | list of key-value pairs to build the trie @t@
  :: Int `BoundedBy` 7
  -> BoundedList 6 (BString4, Int `BoundedBy` 9)
  -> PropertyCheck
prop_fromList_foldl (BoundedBy sparseness) bkvs =
  let kvs = map (first shiftString) $ coerce bkvs :: [(String, Int)]
   in propertyEqWithContext
        (unlines ["kvs = " ++ show kvs])
        (fromListWithSparseness (sparseness - 1) kvs)
        (foldl (flip $ uncurry insert) empty kvs)
  where
    -- Bring characters next to the end of the ASCII range to test the
    -- array-based implementation of fromList.
    shiftString = map shiftChar
    shiftChar = charPlus (126 - fromEnum 'a')
    charPlus x c = toEnum (x + fromEnum c)
