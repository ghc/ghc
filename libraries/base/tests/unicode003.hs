{-# LANGUAGE BlockArguments, LambdaCase #-}

-------------------------------------------------------------------------------
-- |
-- Description: Test general category and simple case mappings for all characters.
--
-- Checking the exact value of these properties for each character would result
-- in a CSV-like file of several MiB. Thus we perform only a /sanity check/
-- using the following method:
--
-- * Group characters in chunks of 100 elements;
-- * For each chunk, compute the hash of each property and create an entry;
-- * An entry is in the format:
--   @<codepoint of first item in chunk>,<hash of prop 1>,<hash of prop 2>,…@
--   (see 'header' for more details).
--
-- Reference test output (@unicode003.stdout@) was initially generated with:
--
-- > runghc-9.2.2 unicode003.hs > unicode003.stdout
--
-- and compared to Python 3.11.0 using @ucd2haskell/tests/check_test_data.py@
--
-------------------------------------------------------------------------------

module Main where

import Data.Bits
import Data.Char
import Data.Foldable
import Data.Int
import Data.List (unfoldr)
import Numeric

-- | File header
header :: String
header = "First Char,GeneralCategory Digest,Case Mappings Digest"

type Chunk a = [a]

-- | Break a list into chunks of given maximum size.
chunksOf :: Int -> [a] -> [Chunk a]
chunksOf n = unfoldr \case
  [] -> Nothing
  l  -> Just (splitAt n l)

type Salt   = Int32
type Digest = Int32

-- [NOTE] adapted from “hashable” package (see: Data.Hashable.LowLevel).
-- Use salt 16777619 for 32 bits and 1099511628211 for 64 bits.
-- | Compute the hash of an integer given a salt.
hashInt :: Salt -> Digest -> Salt
hashInt s x = (s * 16777619) `xor` x

-- | Compute the hash of 'generalCategory'.
generalCategoryHash :: Char -> Digest
generalCategoryHash = fromIntegral . fromEnum . generalCategory

-- | Compute the hash of a character’s case mappings
-- ('toLower', 'toUpper', 'toTitle').
caseMappingsHash :: Char -> Digest
caseMappingsHash c = fromIntegral (ord (toLower c))
           `hashInt` fromIntegral (ord (toUpper c))
           `hashInt` fromIntegral (ord (toTitle c))

-- | Compute the hash of a character with a given conversion function and salt.
hashUsing :: (Char -> Digest) -> Salt -> Char -> Digest
hashUsing f s = hashInt s . f

-- | Make a CSV entry for a chunk.
mkEntry :: Chunk Char -> String
mkEntry []       = mempty
mkEntry cs@(c:_) =
  -- Compute chunk’s hash by chaining hash computations
  let gcDigest = foldl' (hashUsing generalCategoryHash) (generalCategoryHash c) cs
      cmDigest = foldl' (hashUsing caseMappingsHash)    (caseMappingsHash    c) cs
  -- Create CSV line
  in showHex (ord c) (',' : shows gcDigest (',' : show cmDigest))

main :: IO ()
main = do
  putStrLn header
  traverse_ (putStrLn . mkEntry) (chunksOf 100 [minBound..maxBound])
