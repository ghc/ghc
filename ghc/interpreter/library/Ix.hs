#ifdef HEAD
module Ix ( Ix(range, index, inRange), rangeSize ) where
import PreludeBuiltin
#endif /* HEAD */
#ifdef BODY

class  (Show a, Ord a) => Ix a  where
    range               :: (a,a) -> [a]
    index               :: (a,a) -> a -> Int
    inRange             :: (a,a) -> a -> Bool

rangeSize :: Ix a => (a,a) -> Int
rangeSize b@(l,h) | l > h     = 0
                  | otherwise = index b h + 1 
 
#if STD_PRELUDE
#else
instance  Ix Bool  where
    range (c,c')        =  [c..c']
    index b@(c,c') ci
        | inRange b ci  =  fromEnum ci - fromEnum c
        | otherwise     =  error "Ix.index.Bool: Index out of range."
    inRange (c,c') ci   =  fromEnum c <= i && i <= fromEnum c'
                           where i = fromEnum ci
#endif

instance  Ix Char  where
    range (c,c')        =  [c..c']
    index b@(c,c') ci
        | inRange b ci  =  fromEnum ci - fromEnum c
        | otherwise     =  error "Ix.index.Char: Index out of range."
    inRange (c,c') ci   =  fromEnum c <= i && i <= fromEnum c'
                           where i = fromEnum ci

instance  Ix Int  where
    range (m,n)         =  [m..n]
    index b@(m,n) i
        | inRange b i   =  i - m
        | otherwise     =  error "Ix.index.Int: Index out of range."
    inRange (m,n) i     =  m <= i && i <= n

#ifdef PROVIDE_INTEGER
instance  Ix Integer  where
    range (m,n)         =  [m..n]
    index b@(m,n) i
#if STD_PRELUDE
        | inRange b i   =  fromInteger (i - m)
#else
                           /* fromInteger may not have an Integer arg :-) */
        | inRange b i   =  toInt (i - m)
#endif
        | otherwise     =  error "Ix.index.Integer: Index out of range."
    inRange (m,n) i     =  m <= i && i <= n
#endif

#if STD_PRELUDE
instance (Ix a,Ix b) => Ix (a, b) -- as derived, for all tuples
instance Ix Bool                  -- as derived
instance Ix Ordering              -- as derived
instance Ix ()                    -- as derived
#else
-- #error "Missing Ix instances"
#endif

#endif /* BODY */