{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}
module T9263b where

import Data.Proxy

class kproxy ~ 'KProxy => PEq (kproxy :: KProxy a) where
   type F (x :: a) :: Bool
   type F (x :: a) = False
