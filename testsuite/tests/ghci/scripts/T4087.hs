{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE GADTs #-}

module T4087 where

data Equal a b where
     Equal :: Equal a a
