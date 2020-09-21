{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
module T18723 where

import Data.Kind
import Data.Proxy

data T1 = MkT1
  ( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int
  )

data T2 = MkT2 (Proxy
 '( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int
  ))

data T3 = MkT3
 (# Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int
  #)

data T4 = MkT4 (Proxy
 (( Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int
  , Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int
  , Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int
  , Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int
  , Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int
  , Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int, Show Int
  , Show Int, Show Int, Show Int
  )))

f ::
  ( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int
  )
f =
  ( 123, 123, 123, 123, 123, 123, 123, 123, 123, 123
  , 123, 123, 123, 123, 123, 123, 123, 123, 123, 123
  , 123, 123, 123, 123, 123, 123, 123, 123, 123, 123
  , 123, 123, 123, 123, 123, 123, 123, 123, 123, 123
  , 123, 123, 123, 123, 123, 123, 123, 123, 123, 123
  , 123, 123, 123, 123, 123, 123, 123, 123, 123, 123
  , 123, 123, 123
  )

data T6 = MkT6
  ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
   Int Int Int Int Int Int Int Int Int Int
   Int Int Int Int Int Int Int Int Int Int
   Int Int Int Int Int Int Int Int Int Int
   Int Int Int Int Int Int Int Int Int Int
   Int Int Int Int Int Int Int Int Int Int
   Int Int Int Int Int Int Int Int Int Int
   Int Int Int)
