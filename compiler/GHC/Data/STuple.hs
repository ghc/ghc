{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Defines a strict tuple data types, such as 'SPair'.
module GHC.Data.STuple
  ( SPair(..), pattern (:*:), swap, toPair
  , STriple(..), toTriple
  ) where

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

-- | Strict triple data type
data STriple a b c = S3 { sFstOf3 :: !a, sSndOf3 :: !b, sTrdOf3 :: !c }

toTriple :: STriple a b c -> (a, b, c)
toTriple (S3 a b c) = (a, b, c)
