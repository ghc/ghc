{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

module T20642 where

foo :: [a] -> [a]
foo [] = []
foo xs = ys
  where
  (_, ys@(_:_)) = splitAt 0 xs

-- Should case split on the Maybe
f :: Maybe a -> ()
f x = case x of {}

-- Should *not* case split on the Bool
g :: (Bool, [a]) -> ()
g (_, []) = ()
