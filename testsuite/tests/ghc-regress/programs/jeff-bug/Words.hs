module Words where

import qualified Word
import Int
import Ix

-- Begin Signature ---------------------------------------------------------

{- 
The Word class captures both the common operations and class instances
that you would want from words of different sizes.  
-}

class (Ix w,Num w,Integral w,Bounded w, Eq w) => Word w where
  --intToWord       :: Int -> w
  num_half        :: w
  num_bytes       :: w
  max_signed      :: w
  min_signed      :: w
  max_signed_half :: w
  sign            :: w -> w
  unsign          :: w -> w
  --toWord          :: Integral a => a -> w
  sign_half       :: w -> w

  --toWord      = intToWord . toInt
  sign_half n = n `signedModulus` num_half

class Word w => Word2 i w where
  toWord :: i -> w

{-instance Word Int-}
{-instance Word Word8-}
{-instance Word Word32-}

-- This isn't set yet because Word64 is not set to Num in Hugs
-- instance Word Word64 

-- End Signature ---------------------------------------------------------




instance Word Int where
  num_half        = mk_num_half 31
  num_bytes       = mk_num_bytes 31
  max_signed      = mk_max_signed 31
  min_signed      = mk_min_signed 31
  max_signed_half = mk_max_signed_half 31
  sign            = mk_sign 31
  unsign          = mk_unsign 31

instance Integral i => Word2 i Int where
  toWord          = toInt

instance Word Word.Word8 where
  num_half        = mk_num_half 8 
  num_bytes       = mk_num_bytes 8      
  max_signed      = mk_max_signed 8
  min_signed      = mk_min_signed 8
  max_signed_half = mk_max_signed_half 8
  sign            = mk_sign 8
  unsign          = mk_unsign 8

instance Integral i => Word2 i Word.Word8 where
  toWord          = fromIntegral{-intToWord8-} . toInt


instance Word Word.Word32 where
  num_half        = mk_num_half 32
  num_bytes       = mk_num_bytes 32      
  max_signed      = mk_max_signed 32
  min_signed      = mk_min_signed 32
  max_signed_half = mk_max_signed_half 32
  sign            = mk_sign 32
  unsign          = mk_unsign 32

instance Integral i => Word2 i Word.Word32 where
  toWord          = fromIntegral{-intToWord32-} . toInt

mk_num_half  x       = 2^(x `div` 2)
mk_num_bytes x       = 2^(x `div` 4)
mk_max_signed x      = 2^(x-1) - 1
mk_min_signed x      = -2^(x-1)
mk_max_signed_half x = 2^((x `div` 2) - 1) - 1
mk_sign x n          = fromInteger $ n' `signedModulus` ((2^x)::Integer)
   where n' = toInteger n
mk_unsign x n        = fromInteger $ if n' >=0 then n' else n' + 2^x'
   where n' = toInteger n
         x' = toInteger x 

signedModulus x m
  = if modNum <= (m `div` 2) - 1
      then modNum
      else m - modNum
    where
      modNum = x `mod` m


