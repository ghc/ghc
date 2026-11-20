-- | Ordinary list consumers of a stream with hidden state.
module StreamFold (first, firstMapped, digits) where

import ErasedStream (Stream, mapToList, toList)

-- | Return the first element without inspecting the remaining stream.
first :: Stream a -> Maybe a
first s = foldr (\x _ -> Just x) Nothing (toList s)

-- | Read the first yielded digit as the units digit.
digits :: Stream Integer -> Integer
digits s = foldr (\x suffix -> x + 10 * suffix) 0 (toList s)

-- | Apply the function to the first yielded element.
firstMapped :: (a -> b) -> Stream a -> Maybe b
firstMapped f s = foldr (\x _ -> Just x) Nothing (mapToList f s)
