-- | Benchmarking utilities.  For example, functions for generating
-- random 'ByteString's.
module Util.ByteString where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C

import Util.String as String

-- | Generate a number of fixed length 'ByteString's where the content
-- of the strings are letters in ascending order.
asc :: Int  -- ^ Length of each string
    -> Int  -- ^ Number of strings
    -> [S.ByteString]
asc strlen num = map C.pack $ String.asc strlen num

-- | Generate a number of fixed length 'ByteString's where the content
-- of the strings are letters in random order.
rnd :: Int  -- ^ Length of each string
    -> Int  -- ^ Number of strings
    -> [S.ByteString]
rnd strlen num = map C.pack $ String.rnd strlen num

-- | Generate a number of fixed length 'ByteString's where the content
-- of the strings are letters in random order, different from @rnd@.
rnd' :: Int  -- ^ Length of each string
     -> Int  -- ^ Number of strings
     -> [S.ByteString]
rnd' strlen num = map C.pack $ String.rnd' strlen num
