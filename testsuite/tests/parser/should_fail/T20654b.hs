
{-# LANGUAGE ImplicitParams, ImpredicativeTypes #-}

module T20654b where

bar :: (?ip1 :: ?ip2 :: Int) => Int
bar = ?ip2
