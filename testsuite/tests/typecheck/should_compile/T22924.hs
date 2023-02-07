{-# LANGUAGE FlexibleInstances #-}
module G where

import Data.Functor.Const( Const )
import Data.Coerce

f :: Coercible (f a) a => Const a () -> Const (f a) ()
f = coerce

