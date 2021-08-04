-- | Benchmarking utilities.  For example, functions for generating
-- random strings.
module Util.String where

import System.Random (mkStdGen, randomRs)

-- | Generate a number of fixed length strings where the content of
-- the strings are letters in ascending order.
asc :: Int  -- ^ Length of each string
    -> Int  -- ^ Number of strings
    -> [String]
asc strlen num = take num $ iterate (snd . inc) $ replicate strlen 'a'
  where inc [] = (True, [])
        inc (c:cs) = case inc cs of (True, cs') | c == 'z'  -> (True, 'a' : cs')
                                                | otherwise -> (False, succ c : cs')
                                    (False, cs')            -> (False, c : cs')

-- | Generate a number of fixed length strings where the content of
-- the strings are letters in random order.
rnd :: Int  -- ^ Length of each string
    -> Int  -- ^ Number of strings
    -> [String]
rnd strlen num = take num $ split $ randomRs ('a', 'z') $ mkStdGen 1234
  where
    split cs = case splitAt strlen cs of (str, cs') -> str : split cs'

-- | Generate a number of fixed length strings where the content of
-- the strings are letters in random order, different from rnd
rnd' :: Int  -- ^ Length of each string
     -> Int  -- ^ Number of strings
     -> [String]
rnd' strlen num = take num $ split $ randomRs ('a', 'z') $ mkStdGen 5678
  where
    split cs = case splitAt strlen cs of (str, cs') -> str : split cs'
