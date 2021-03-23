{-# LANGUAGE PatternSynonyms #-}

-- | Defines a strict tuple data types, such as 'SPair'.
module GHC.Data.STuple
  ( SPair(..), pattern (:*:), swap, toPair, sFirst, sSecond, sUnzip
  , STriple(..), mapSSndOf3, mapSTrdOf3, toTriple
  , SQuad(..), toQuad
  ) where

import GHC.Prelude

-- | Strict pair data type
data SPair a b = S2 { sFst :: !a, sSnd :: !b }

-- | Infix alternative to 'S2'.
pattern (:*:) :: a -> b -> SPair a b
pattern a :*: b = S2 a b
{-# COMPLETE (:*:) #-}
infixr 1 :*:

swap :: SPair a b -> SPair b a
swap (S2 a b) = (S2 b a)

toPair :: SPair a b -> (a, b)
toPair (S2 a b) = (a, b)

sFirst :: (a -> a') -> SPair a b -> SPair a' b
sFirst f (S2 a b) = S2 (f a) b

sSecond :: (b -> b') -> SPair a b -> SPair a b'
sSecond f (S2 a b) = S2 a (f b)

sUnzip :: [SPair a b] -> SPair [a] [b]
sUnzip = uncurry S2 . unzip . map toPair

-- | Strict triple data type
data STriple a b c = S3 { sFstOf3 :: !a, sSndOf3 :: !b, sTrdOf3 :: !c }

mapSSndOf3 :: (b -> b') -> STriple a b c -> STriple a b' c -- feel free to add more as needed
mapSSndOf3 f (S3 a b c) = S3 a (f b) c

mapSTrdOf3 :: (c -> c') -> STriple a b c -> STriple a b c' -- feel free to add more as needed
mapSTrdOf3 f (S3 a b c) = S3 a b (f c)

toTriple :: STriple a b c -> (a, b, c)
toTriple (S3 a b c) = (a, b, c)

-- | Strict quadruple data type
data SQuad a b c d = S4 { sFstOf4 :: !a, sSndOf4 :: !b, sTrdOf4 :: !c, sFthOf4 :: !d }

toQuad :: SQuad a b c d -> (a, b, c, d)
toQuad (S4 a b c d) = (a, b, c, d)
