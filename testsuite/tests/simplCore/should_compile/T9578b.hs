{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}

module Main where

import Data.Array.Byte
  ( ByteArray(..) )
import Data.Ix
  ( Ix, range, index, rangeSize )
import GHC.Show
  ( appPrec )
import GHC.Exts
  ( Int(..), ByteArray#, indexWord64Array# )
import GHC.IsList
  ( fromList )
import GHC.Word
  ( Word8, Word64(..) )

data UArray i e = UArray !i !i !Int ByteArray#
type role UArray nominal nominal

class IArray a e where
    bounds           :: Ix i => a i e -> (i,i)
    numElements      :: Ix i => a i e -> Int
    unsafeAt         :: Ix i => a i e -> Int -> e

{-# INLINE safeRangeSize #-}
safeRangeSize :: Ix i => (i, i) -> Int
safeRangeSize (l,u) = let r = rangeSize (l, u)
                      in if r < 0 then error "Negative range size"
                                  else r

{-# INLINE safeIndex #-}
safeIndex :: Ix i => (i, i) -> Int -> i -> Int
safeIndex (l,u) n i = let i' = index (l,u) i
                      in if (0 <= i') && (i' < n)
                         then i'
                         else error ("Error in array index; " ++ show i' ++
                                     " not in range [0.." ++ show n ++ ")")

{-# INLINE (!) #-}
-- | Returns the element of an immutable array at the specified index,
-- or throws an exception if the index is out of bounds.
(!) :: (IArray a e, Ix i) => a i e -> i -> e
(!) arr i = case bounds arr of
              (l,u) -> unsafeAt arr $ safeIndex (l,u) (numElements arr) i


{-# INLINE assocs #-}
-- | Returns the contents of an array as a list of associations.
assocs :: (IArray a e, Ix i) => a i e -> [(i, e)]
assocs arr = case bounds arr of
    (l,u) -> [(i, arr ! i) | i <- range (l,u)]

showsIArray :: (IArray a e, Ix i, Show i, Show e) => Int -> a i e -> ShowS
showsIArray p a =
    showParen (p > appPrec) $
    showString "array " .
    shows (bounds a) .
    showChar ' ' .
    shows (assocs a)

{-# SPECIALISE
    showsIArray :: (IArray UArray e, Ix i, Show i, Show e) =>
                   Int -> UArray i e -> ShowS
  #-}

instance IArray UArray Word64 where
    {-# INLINE bounds #-}
    bounds (UArray l u _ _) = (l,u)
    {-# INLINE numElements #-}
    numElements (UArray _ _ n _) = n
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ _ arr#) (I# i#) = W64# (indexWord64Array# arr# i#)

instance (Ix ix, Show ix, Show e, IArray UArray e) => Show (UArray ix e) where
    showsPrec = showsIArray


main :: IO ()
main = do
  let ba :: ByteArray#
      ByteArray ba = fromList ( replicate (2 * 8) 0 :: [ Word8 ] )
  print (UArray 0 1 2 ba :: UArray Int Word64)
