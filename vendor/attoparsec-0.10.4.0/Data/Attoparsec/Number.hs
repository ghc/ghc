{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      :  Data.Attoparsec.Number
-- Copyright   :  Bryan O'Sullivan 2011
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- A simple number type, useful for parsing both exact and inexact
-- quantities without losing much precision.
module Data.Attoparsec.Number
    (
      Number(..)
    ) where

import Control.DeepSeq (NFData(rnf))
import Data.Data (Data)
import Data.Function (on)
import Data.Typeable (Typeable)

-- | A numeric type that can represent integers accurately, and
-- floating point numbers to the precision of a 'Double'.
data Number = I !Integer
            | D {-# UNPACK #-} !Double
              deriving (Typeable, Data)

instance Show Number where
    show (I a) = show a
    show (D a) = show a

instance NFData Number where
    rnf (I _) = ()
    rnf (D _) = ()
    {-# INLINE rnf #-}

binop :: (Integer -> Integer -> a) -> (Double -> Double -> a)
      -> Number -> Number -> a
binop _ d (D a) (D b) = d a b
binop i _ (I a) (I b) = i a b
binop _ d (D a) (I b) = d a (fromIntegral b)
binop _ d (I a) (D b) = d (fromIntegral a) b
{-# INLINE binop #-}

instance Eq Number where
    (==) = binop (==) (==)
    {-# INLINE (==) #-}

    (/=) = binop (/=) (/=)
    {-# INLINE (/=) #-}

instance Ord Number where
    (<) = binop (<) (<)
    {-# INLINE (<) #-}

    (<=) = binop (<=) (<=)
    {-# INLINE (<=) #-}

    (>) = binop (>) (>)
    {-# INLINE (>) #-}

    (>=) = binop (>=) (>=)
    {-# INLINE (>=) #-}

    compare = binop compare compare
    {-# INLINE compare #-}

instance Num Number where
    (+) = binop (((I$!).) . (+)) (((D$!).) . (+))
    {-# INLINE (+) #-}

    (-) = binop (((I$!).) . (-)) (((D$!).) . (-))
    {-# INLINE (-) #-}

    (*) = binop (((I$!).) . (*)) (((D$!).) . (*))
    {-# INLINE (*) #-}

    abs (I a) = I $! abs a
    abs (D a) = D $! abs a
    {-# INLINE abs #-}

    negate (I a) = I $! negate a
    negate (D a) = D $! negate a
    {-# INLINE negate #-}

    signum (I a) = I $! signum a
    signum (D a) = D $! signum a
    {-# INLINE signum #-}

    fromInteger = (I$!) . fromInteger
    {-# INLINE fromInteger #-}

instance Real Number where
    toRational (I a) = fromIntegral a
    toRational (D a) = toRational a
    {-# INLINE toRational #-}

instance Fractional Number where
    fromRational = (D$!) . fromRational
    {-# INLINE fromRational #-}

    (/) = binop (((D$!).) . (/) `on` fromIntegral)
                (((D$!).) . (/))
    {-# INLINE (/) #-}

    recip (I a) = D $! recip (fromIntegral a)
    recip (D a) = D $! recip a
    {-# INLINE recip #-}

instance RealFrac Number where
    properFraction (I a) = (fromIntegral a,0)
    properFraction (D a) = case properFraction a of
                             (i,d) -> (i,D d)
    {-# INLINE properFraction #-}
    truncate (I a) = fromIntegral a
    truncate (D a) = truncate a
    {-# INLINE truncate #-}
    round (I a) = fromIntegral a
    round (D a) = round a
    {-# INLINE round #-}
    ceiling (I a) = fromIntegral a
    ceiling (D a) = ceiling a
    {-# INLINE ceiling #-}
    floor (I a) = fromIntegral a
    floor (D a) = floor a
    {-# INLINE floor #-}
