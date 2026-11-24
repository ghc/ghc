{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
-- | An isomorphism whose two functions appear in its proof fields.
module BookIsomorphism (Iso(..), Bit(..), sample) where

import Data.Type.Equality ((:~:)(Refl))

-- | Two functions that are inverses for every input.
data Iso a b where
    MkIso :: foreach (to :: a -> b) -> foreach (from :: b -> a)
          -> (foreach (x :: a) -> from (to x) :~: x)
          -> (foreach (y :: b) -> to (from y) :~: y)
          -> Iso a b

-- | Store the two functions with their pointwise inverse laws.
pack :: foreach (to :: a -> b) -> foreach (from :: b -> a)
     -> (foreach (x :: a) -> from (to x) :~: x)
     -> (foreach (y :: b) -> to (from y) :~: y)
     -> Iso a b
pack to from left right = MkIso to from left right

-- | Two possible bit values.
data Bit = Clear | Set deriving Show

-- | Encode a Boolean as a bit.
encode :: Bool -> Bit
encode False = Clear
encode True = Set

-- | Decode a bit as a Boolean.
decode :: Bit -> Bool
decode Clear = False
decode Set = True

-- | Decoding an encoded Boolean returns the input.
decodeEncode :: foreach (x :: Bool) -> decode (encode x) :~: x
decodeEncode False = Refl
decodeEncode True = Refl

-- | Encoding a decoded bit returns the input.
encodeDecode :: foreach (x :: Bit) -> encode (decode x) :~: x
encodeDecode Clear = Refl
encodeDecode Set = Refl

-- | The isomorphism between Booleans and bits.
sample :: Iso Bool Bit
sample = pack encode decode decodeEncode encodeDecode
