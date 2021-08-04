{-# LANGUAGE CPP #-}
module Basement.Compat.ExtList
    ( length
    , null
    , sum
    , reverse
    , (!!)
    ) where

import Basement.Compat.Base
import Basement.Numerical.Additive
import Basement.Types.OffsetSize
import qualified GHC.List as List

-- | Compute the size of the list
length :: [a] -> CountOf a
#if MIN_VERSION_base(4,8,0)
length = CountOf . List.foldl' (\c _ -> c+1) 0
#else
length = CountOf . loop 0
  where loop !acc []     = acc
        loop !acc (_:xs) = loop (1+acc) xs
#endif

null :: [a] -> Bool
null []    = True
null (_:_) = False

-- | Sum the element in a list
sum :: Additive n => [n] -> n
sum []     = azero
sum (i:is) = loop i is
  where
    loop !acc [] = acc
    loop !acc (x:xs) = loop (acc+x) xs
    {-# INLINE loop #-}

reverse :: [a] -> [a]
reverse l =  go l []
  where
    go []     acc = acc
    go (x:xs) acc = go xs (x:acc)

(!!) :: [a] -> Offset a -> a
[]    !! _  = error "invalid offset for !!"
(x:_) !! 0  = x
(_:xs) !! i = xs !! pred i
