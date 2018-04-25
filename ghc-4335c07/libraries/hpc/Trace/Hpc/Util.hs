{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------
-- Andy Gill and Colin Runciman, June 2006
------------------------------------------

-- | Minor utilities for the HPC tools.

module Trace.Hpc.Util
       ( HpcPos
       , fromHpcPos
       , toHpcPos
       , insideHpcPos
       , HpcHash(..)
       , Hash
       , catchIO
       ) where

import qualified Control.Exception as Exception
import Data.List(foldl')
import Data.Char (ord)
import Data.Bits (xor)
import Data.Word

-- | 'HpcPos' is an Hpc local rendition of a Span.
data HpcPos = P !Int !Int !Int !Int deriving (Eq, Ord)

-- | 'fromHpcPos' explodes the HpcPos into /line:column/-/line:column/
fromHpcPos :: HpcPos -> (Int,Int,Int,Int)
fromHpcPos (P l1 c1 l2 c2) = (l1,c1,l2,c2)

-- | 'toHpcPos' implodes to HpcPos, from /line:column/-/line:column/
toHpcPos :: (Int,Int,Int,Int) -> HpcPos
toHpcPos (l1,c1,l2,c2) = P l1 c1 l2 c2

-- | Predicate determining whether the first argument is inside the second argument.
insideHpcPos :: HpcPos -> HpcPos -> Bool
insideHpcPos small big =
             sl1 >= bl1 &&
             (sl1 /= bl1 || sc1 >= bc1) &&
             sl2 <= bl2 &&
             (sl2 /= bl2 || sc2 <= bc2)
  where (sl1,sc1,sl2,sc2) = fromHpcPos small
        (bl1,bc1,bl2,bc2) = fromHpcPos big

instance Show HpcPos where
   show (P l1 c1 l2 c2) = show l1 ++ ':' : show c1 ++ '-' : show l2 ++ ':' : show c2

instance Read HpcPos where
  readsPrec _i pos = [(toHpcPos (read l1,read c1,read l2,read c2),after)]
      where
         (before,after)   = span (/= ',') pos
         (lhs0,rhs0)    = case span (/= '-') before of
                               (lhs,'-':rhs) -> (lhs,rhs)
                               (lhs,"")      -> (lhs,lhs)
                               _ -> error "bad parse"
         (l1,':':c1)      = span (/= ':') lhs0
         (l2,':':c2)      = span (/= ':') rhs0

------------------------------------------------------------------------------

-- Very simple Hash number generators

class HpcHash a where
  toHash :: a -> Hash

newtype Hash = Hash Word32 deriving (Eq)

instance Read Hash where
  readsPrec p n = [ (Hash v,rest)
                  | (v,rest) <- readsPrec p n
                  ]

instance Show Hash where
  showsPrec p (Hash n) = showsPrec p n

instance Num Hash where
  (Hash a) + (Hash b) = Hash $ a + b
  (Hash a) * (Hash b) = Hash $ a * b
  (Hash a) - (Hash b) = Hash $ a - b
  negate (Hash a)     = Hash $ negate a
  abs (Hash a)        = Hash $ abs a
  signum (Hash a)     = Hash $ signum a
  fromInteger n       = Hash $ fromInteger n

instance HpcHash Int where
  toHash n = Hash $ fromIntegral n

instance HpcHash Integer where
  toHash n = fromInteger n

instance HpcHash Char where
  toHash c = Hash $ fromIntegral $ ord c

instance HpcHash Bool where
  toHash True  = 1
  toHash False = 0

instance HpcHash a => HpcHash [a] where
  toHash xs = foldl' (\ h c -> toHash c `hxor` (h * 33)) 5381 xs

instance (HpcHash a,HpcHash b) => HpcHash (a,b) where
  toHash (a,b) = (toHash a * 33) `hxor` toHash b

instance HpcHash HpcPos where
  toHash (P a b c d) = Hash $ fromIntegral $ a * 0x1000000 + b * 0x10000 + c * 0x100 + d

hxor :: Hash -> Hash -> Hash
hxor (Hash x) (Hash y) = Hash $ x `xor` y

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch
