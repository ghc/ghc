{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wincomplete-record-selectors #-}

module T26686 where

import Data.Kind

data A
data B

data G = G { f2 :: Int }

data T x where
  TA :: { ta :: G } -> T x
  TB :: { tb :: G } -> T B

data H a = H { f1 :: T a }

test1_ok :: T A -> G
test1_ok = (.ta)
test2_ok :: T A -> Int
test2_ok = (.ta.f2)
test3_ok :: H A -> G
test3_ok = (.f1.ta)
test4_ok :: H A -> Int
test4_ok = (.f1.ta.f2)

test1_bad :: T x -> G
test1_bad = (.ta)
test2_bad :: T x -> Int
test2_bad = (.ta.f2)
test3_bad :: H x -> G
test3_bad = (.f1.ta)
test4_bad :: H x -> Int
test4_bad = (.f1.ta.f2)
