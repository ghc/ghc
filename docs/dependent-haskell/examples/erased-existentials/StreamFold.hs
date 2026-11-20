-- | Ordinary list consumers of a stream with hidden state.
module StreamFold (first, digits) where

import ErasedStream (Stream, toList)

-- | Return the first element without inspecting the remaining stream.
first :: Stream a -> Maybe a
first s = foldr (\x _ -> Just x) Nothing (toList s)

-- | Read the first yielded digit as the units digit.
digits :: Stream Integer -> Integer
digits s = foldr (\x suffix -> x + 10 * suffix) 0 (toList s)
