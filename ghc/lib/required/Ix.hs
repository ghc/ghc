module Ix ( Ix(range, index, inRange) ) where

class  (Show a, Ord a) => Ix a  where
    range		:: (a,a) -> [a]
    index		:: (a,a) -> a -> Int
    inRange		:: (a,a) -> a -> Bool

instance  Ix Char  where
    range (c,c')	=  [c..c']
    index b@(c,c') ci
	| inRange b ci	=  fromEnum ci - fromEnum c
	| otherwise	=  error "LibIx.index: Index out of range."
    inRange (c,c') ci	=  fromEnum c <= i && i <= fromEnum c'
			   where i = fromEnum ci

instance  Ix Int  where
    range (m,n)		=  [m..n]
    index b@(m,n) i
	| inRange b i	=  i - m
	| otherwise	=  error "LibIx.index: Index out of range."
    inRange (m,n) i	=  m <= i && i <= n

instance  Ix Integer  where
    range (m,n)		=  [m..n]
    index b@(m,n) i
	| inRange b i	=  fromInteger (i - m)
	| otherwise	=  error "LibIx.index: Index out of range."
    inRange (m,n) i	=  m <= i && i <= n

----------------------------------------------------------------------
instance Ix Bool where -- as derived
    range   (l,u)   = map toEnum [fromEnum l .. fromEnum u]
    index   (l,u) i = fromEnum i - fromEnum l
    inRange (l,u) i = fromEnum i >= fromEnum l && fromEnum i <= fromEnum u

----------------------------------------------------------------------
instance Ix Ordering where -- as derived
    range   (l,u)   = map toEnum [fromEnum l .. fromEnum u]
    index   (l,u) i = fromEnum i - fromEnum l
    inRange (l,u) i = fromEnum i >= fromEnum l && fromEnum i <= fromEnum u


----------------------------------------------------------------------
instance Ix () where
    {-# INLINE range #-}
    range   ((), ())    = [()]
    {-# INLINE index #-}
    index   ((), ()) () = 0
    {-# INLINE inRange #-}
    inRange ((), ()) () = True

----------------------------------------------------------------------
instance (Ix a, Ix b) => Ix (a, b) where -- as derived
    {-# INLINE range #-}
    range ((l1,l2),(u1,u2))
      = [ (i1,i2) | i1 <- range (l1,u1), i2 <- range (l2,u2) ]

    {-# INLINE index #-}
    index ((l1,l2),(u1,u2)) (i1,i2)
      = index (l1,u1) i1 * (index (l2,u2) u2 + (I# 1#)){-rangeSize (l2,u2)-} + index (l2,u2) i2

    {-# INLINE inRange #-}
    inRange ((l1,l2),(u1,u2)) (i1,i2)
      = inRange (l1,u1) i1 && inRange (l2,u2) i2

instance  (Ix a1, Ix a2, Ix a3) => Ix (a1,a2,a3)  where
    range ((l1,l2,l3),(u1,u2,u3)) =
        [(i1,i2,i3) | i1 <- range (l1,u1),
                      i2 <- range (l2,u2),
                      i3 <- range (l3,u3)]

    index ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
      index (l3,u3) i3 + rangeSize (l3,u3) * (
       index (l2,u2) i2 + rangeSize (l2,u2) * (
         index (l1,u1) i1))
      where
	rangeSize (l,u) = index (l,u) u + (1 :: Int)

    inRange ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
        inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
            inRange (l3,u3) i3

instance  (Ix a1, Ix a2, Ix a3, Ix a4) => Ix (a1,a2,a3,a4)  where
    range ((l1,l2,l3,l4),(u1,u2,u3,u4)) =
        [(i1,i2,i3,i4) | i1 <- range (l1,u1),
                         i2 <- range (l2,u2),
                         i3 <- range (l3,u3),
                         i4 <- range (l4,u4)]

    index ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
      index (l4,u4) i4 + rangeSize (l4,u4) * (
       index (l3,u3) i3 + rangeSize (l3,u3) * (
         index (l2,u2) i2 + rangeSize (l2,u2) * (
           index (l1,u1) i1)))
      where
	rangeSize (l,u) = index (l,u) u + (1 :: Int)

    inRange ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
        inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
           inRange (l3,u3) i3 && inRange (l4,u4) i4

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5) => Ix (a1,a2,a3,a4,a5)  where
    range ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) =
        [(i1,i2,i3,i4,i5) | i1 <- range (l1,u1),
                            i2 <- range (l2,u2),
                            i3 <- range (l3,u3),
                            i4 <- range (l4,u4),
                            i5 <- range (l5,u5)]

    index ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
      index (l5,u5) i5 + rangeSize (l5,u5) * (
        index (l4,u4) i4 + rangeSize (l4,u4) * (
         index (l3,u3) i3 + rangeSize (l3,u3) * (
           index (l2,u2) i2 + rangeSize (l2,u2) * (
             index (l1,u1) i1))))
      where
	rangeSize (l,u) = index (l,u) u + (1 :: Int)

    inRange ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
        inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
            inRange (l3,u3) i3 && inRange (l4,u4) i4 && inRange (l5,u5) i5
