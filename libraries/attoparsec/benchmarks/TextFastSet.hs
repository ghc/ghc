{-# LANGUAGE BangPatterns #-}

------------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.FastSet
-- Copyright   :  Felipe Lessa 2010, Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Fast set membership tests for 'Char' values. We test for
-- membership using a hashtable implemented with Robin Hood
-- collision resolution. The set representation is unboxed,
-- and the characters and hashes interleaved, for efficiency.
--
--
-----------------------------------------------------------------------------
module TextFastSet
    (
    -- * Data type
      FastSet
    -- * Construction
    , fromList
    , set
    -- * Lookup
    , member
    -- * Handy interface
    , charClass
    ) where

import Data.Bits ((.|.), (.&.), shiftR)
import Data.Function (on)
import Data.List (sort, sortBy)
import qualified Data.Array.Base as AB
import qualified Data.Array.Unboxed as A
import qualified Data.Text as T

data FastSet = FastSet {
    table :: {-# UNPACK #-} !(A.UArray Int Int)
  , mask  :: {-# UNPACK #-} !Int
  }

data Entry = Entry {
    key          :: {-# UNPACK #-} !Char
  , initialIndex :: {-# UNPACK #-} !Int
  , index        :: {-# UNPACK #-} !Int
  }

offset :: Entry -> Int
offset e = index e - initialIndex e

resolveCollisions :: [Entry] -> [Entry]
resolveCollisions [] = []
resolveCollisions [e] = [e]
resolveCollisions (a:b:entries) = a' : resolveCollisions (b' : entries)
  where (a', b')
          | index a < index b   = (a, b)
          | offset a < offset b = (b { index=index a }, a { index=index a + 1 })
          | otherwise           = (a, b { index=index a + 1 })

pad :: Int -> [Entry] -> [Entry]
pad = go 0
  where -- ensure that we pad enough so that lookups beyond the
        -- last hash in the table fall within the array
        go !_ !m []          = replicate (max 1 m + 1) empty
        go  k  m (e:entries) = map (const empty) [k..i - 1] ++ e :
                               go (i + 1) (m + i - k - 1) entries
          where i            = index e
        empty                = Entry '\0' maxBound 0

nextPowerOf2 :: Int -> Int
nextPowerOf2 0  = 1
nextPowerOf2 x  = go (x - 1) 1
  where go y 32 = y + 1
        go y k  = go (y .|. (y `shiftR` k)) $ k * 2

fastHash :: Char -> Int
fastHash = fromEnum

fromList :: String -> FastSet
fromList s = FastSet (AB.listArray (0, length interleaved - 1) interleaved)
             mask'
  where s'      = ordNub (sort s)
        l       = length s'
        mask'   = nextPowerOf2 ((5 * l) `div` 4) - 1
        entries = pad mask' .
                  resolveCollisions .
                  sortBy (compare `on` initialIndex) .
                  zipWith (\c i -> Entry c i i) s' .
                  map ((.&. mask') . fastHash) $ s'
        interleaved = concatMap (\e -> [fromEnum $ key e, initialIndex e])
                      entries

ordNub :: Eq a => [a] -> [a]
ordNub []     = []
ordNub (y:ys) = go y ys
  where go x (z:zs)
          | x == z    = go x zs
          | otherwise = x : go z zs
        go x []       = [x]

set :: T.Text -> FastSet
set = fromList . T.unpack

-- | Check the set for membership.
member :: Char -> FastSet -> Bool
member c a           = go (2 * i)
  where i            = fastHash c .&. mask a
        lookupAt j b = (i' <= i) && (c == c' || b)
            where c' = toEnum $ AB.unsafeAt (table a) j
                  i' = AB.unsafeAt (table a) $ j + 1
        go j         = lookupAt j . lookupAt (j + 2) . lookupAt (j + 4) .
                       lookupAt (j + 6) . go $ j + 8

charClass :: String -> FastSet
charClass = fromList . go
  where go (a:'-':b:xs) = [a..b] ++ go xs
        go (x:xs)       = x : go xs
        go _            = ""
