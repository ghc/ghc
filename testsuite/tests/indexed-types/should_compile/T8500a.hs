{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies #-}

module T8500a where

class C a where
   type F a
   type F a = [a]
